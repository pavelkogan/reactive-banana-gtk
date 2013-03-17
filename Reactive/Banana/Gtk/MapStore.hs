-- | Things that may not work correctly: what ways do users and other code expect to be able to modify?
module Reactive.Banana.Gtk.MapStore (
    MapStore
  , newMapStore 
  , MapDelta(..)
  , getKeyValueFromIter
  ) where

import Reactive.Banana
import Reactive.Banana.Frameworks
-- import Reactive.Banana.Gtk
-- import Reactive.Banana.Gtk.Collections

import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.TreeModel

import System.Glib.Types (GObjectClass, toGObject, unsafeCastGObject) -- necessary for GObjectClass instance
import Data.IORef
import Control.Monad (liftM)
import Control.Error (assertMay)

import Data.Map (Map)
import qualified Data.Map as Map

-- | A TreeStore backed by Data.Map, using the Ord instance of the keys
newtype MapStore k a = MapStore (CustomStore (IORef (MapStoreState k a)) a)
newtype MapStoreState k a = MapStoreState { currentMap :: Map k a }

instance Ord k => TypedTreeModelClass (MapStore k)
instance Ord k => TreeModelClass (MapStore k a)
instance GObjectClass (MapStore k a) where
  toGObject (MapStore tm) = toGObject tm
  unsafeCastGObject = MapStore . unsafeCastGObject

-- | Because the types do not enforce a distinction between insertions
--   and changes (i.e. whether or not the key was previously in the map), 
--   that distinction is not made here.  (Both MapAdd and MapReplace would have
--   type X k a.  To ensure correct behavior, the update function would have to
--   check whether the key was in the map anyway, making the two constructors
--   uselessly redundant.
data MapDelta k a =
      MapSet k a -- ^ Insert an item that was not previously in the map, or change the value at an existing key.
    | MapRemove k -- ^ Drop an item from the map
    | MapChangeKey k k -- ^ Change the key of an existing item.  If there is no such item, make no change.
    deriving Show
newMapStore :: (Ord k, Frameworks t)
    => Map k a -- ^ Initial data
    -> Event t (MapDelta k a) -- ^ Changes from the event network
    -> Moment t (MapStore k a)
newMapStore initialMap ds = do
    private <- liftIO $ newIORef $ MapStoreState initialMap
    store <- liftIO $ customStoreNew private MapStore (tmIface private) Nothing Nothing
    reactimate $ liftIO . notifyDelta store <$> ds
    return store
  where
    containsMay private [n] = 
        liftM ((n <$) . assertMay . (> n) . Map.size . currentMap) 
            $ readIORef private 
    containsMay _ _ = return Nothing
    toIter i = TreeIter 0 (fromIntegral i) 0 0
    tmIface private = TreeModelIface
        { treeModelIfaceGetFlags = return [TreeModelListOnly]
        , treeModelIfaceGetIter = liftM (fmap toIter) . containsMay private
        , treeModelIfaceGetPath = \(TreeIter _ n _ _) -> return [fromIntegral n]
        , treeModelIfaceGetRow = \(TreeIter _ n _ _) -> liftM 
                (snd . Map.elemAt (fromIntegral n) . currentMap) 
                $ readIORef private
        , treeModelIfaceIterNext = \(TreeIter _ n _ _) -> let n' = (succ . fromIntegral) n in 
                liftM
                    ((toIter n' <$) . assertMay . (> n') . Map.size . currentMap)
                    $ readIORef private
        , treeModelIfaceIterChildren = const $ return Nothing
        
        , treeModelIfaceIterHasChild  = const $ return False
        , treeModelIfaceIterNChildren = \index -> 
                                           case index of
                                             Nothing -> liftM (Map.size . currentMap) $ readIORef private 
                                             _       -> return 0
        , treeModelIfaceIterNthChild  = \index n -> return $ case index of
                                               Nothing -> Just $ toIter n
                                               _       -> Nothing
        , treeModelIfaceIterParent    = const $ return Nothing
        , treeModelIfaceRefNode       = const $ return ()
        , treeModelIfaceUnrefNode     = const $ return ()
        }
-- | Emit the appropriate GTK+ signals
notifyDelta :: (Ord k) 
    => MapStore k a
    -> MapDelta k a 
    -> IO ()
notifyDelta (MapStore self) delta = do
    state <- readIORef $ customStoreGetPrivate self
    let before = currentMap state
        after = applyDelta delta before
    writeIORef (customStoreGetPrivate self) (state { currentMap = after})
    stamp <- customStoreGetStamp self
    case delta of
      MapSet key _ ->
        let i = Map.findIndex key after 
        in case Map.member key before of
            False -> treeModelRowInserted self [i] (TreeIter stamp (fromIntegral i) 0 0)
            True -> treeModelRowChanged self [i] (TreeIter stamp (fromIntegral i) 0 0)
      MapRemove key ->
        let i = Map.findIndex key before
        in case Map.member key before of
            False -> return ()
            True -> treeModelRowDeleted self [i]
      MapChangeKey k1 k2 ->
        let i1 = Map.findIndex k1 before
            i2 = Map.findIndex k2 after
            allItems = [0 .. (Map.size after - 1)]
            oldOrder = do i <- allItems
                          return $ case () of
                            _ | i == i1   -> i2
                            _ | i == i2   -> i1
                            _             -> i
        in treeModelRowsReordered self [] Nothing oldOrder

tryElemAt :: (Ord k) => Int -> Map k v -> Maybe (k, v)
tryElemAt n m = case Map.size m of
    s | s > n -> Just $ Map.elemAt n m
    _ -> Nothing
        
getKeyValueFromIter :: (Ord k) => MapStore k a -> TreeIter -> IO (Maybe (k,a))
getKeyValueFromIter (MapStore self) (TreeIter _ n _ _) = 
    liftM (tryElemAt (fromIntegral n) . currentMap) 
        $ readIORef $ customStoreGetPrivate self

applyDelta :: Ord k => MapDelta k a -> Map k a -> Map k a
applyDelta (MapSet key val) = Map.insert key val
applyDelta (MapRemove key) = Map.delete key
applyDelta (MapChangeKey old new) = \i -> case Map.lookup old i of
                Nothing -> i
                Just v -> Map.insert new v . Map.delete old $ i
     
    
-- getPosition :: Ord k => Map k a -> 
