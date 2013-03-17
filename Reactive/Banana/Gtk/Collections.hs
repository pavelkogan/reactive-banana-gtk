{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Reactive.Banana.Gtk.Collections (
    ColumnBinder,
    bindTreeList, 
    bindTextCol, bindToggleCol,
    TreeBindingSetup,
    Reactive.Banana.Gtk.MapStore.MapDelta(..))
    where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import Reactive.Banana.Gtk.MapStore
import Data.Map (Map)

type ColumnBinder t m v r a = ReaderT (TreeBindingSetup m v r) (Moment t) a

bindTreeList :: (TreeViewClass view, Frameworks t, Ord k)
    => view -- ^ The TreeView to bind
    -> Map k row -- ^ Initial data
    -> Event t (MapDelta k row) -- ^ Changes from the event network
    -> (forall model. ColumnBinder t model view row a) -- ^ Action to bind individual columns
    -> Moment t (Behavior t (Maybe k), a)
bindTreeList view initialMap deltas m = do
    store <- newMapStore initialMap deltas
    liftIO $ treeViewSetModel view store
    a <- runReaderT m $ TreeBindingSetup store
    
    selectionE <-  monitorF view cursorChanged $ const $ do
                        iter <- treeViewGetSelection view >>= treeSelectionGetSelected
                        case iter of Nothing -> return Nothing; Just i -> getKeyFromIter store i
    return (stepper Nothing selectionE, a)                   
data TreeBindingSetup model view row where
    TreeBindingSetup :: (TreeViewClass view,
                         TreeModelClass (model row),
                         TypedTreeModelClass model)
                     => model row 
                     -> TreeBindingSetup model view row

bindCol' :: (Frameworks t)
    => CRenderer a
    -> TreeViewColumn 
    -> (row -> a) 
    -> ColumnBinder t m v row ()
bindCol' r col f = ask >>= \setup -> 
    case setup of -- unwrap the GADT to get our typeclass contexts
        (TreeBindingSetup model) -> liftIO $ mkRenderer r col f model
    
bindTextCol :: (Frameworks t) 
    => TreeViewColumn -- ^ the column to bind
    -> (row -> String) -- ^ accessor
    -> ColumnBinder t m v row ()
bindTextCol = bindCol' CRText
bindToggleCol :: (Frameworks t) => TreeViewColumn -> (row -> Bool) -> ColumnBinder t m v row ()
bindToggleCol = bindCol' CRToggle

data CRenderer t where
    CRText :: CRenderer String
    CRToggle :: CRenderer Bool
mkRenderer 
    :: (TreeModelClass (model row), TypedTreeModelClass model) 
    => CRenderer a 
    -> TreeViewColumn 
    -> (row -> a)
    -> model row
    -> IO ()
mkRenderer CRText col extract model = do
    r <- cellRendererTextNew
    cellLayoutPackStart col r True
    cellLayoutSetAttributes col r model $ \row -> [ cellText := extract row ]
mkRenderer CRToggle col extract model = do
    r <- cellRendererToggleNew
    cellLayoutPackStart col r True
    cellLayoutSetAttributes col r model $ \row -> [ cellToggleActive := extract row ]

