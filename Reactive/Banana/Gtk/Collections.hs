{-# LANGUAGE GADTs, ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
module Reactive.Banana.Gtk.Collections (
    ColumnBinder,
    bindTreeList, 
    bindTextCol, bindToggleCol,
    TreeBindingSetup )
    where

import Graphics.UI.Gtk
import Control.Monad.Reader
import Reactive.Banana
import Reactive.Banana.Frameworks

type ColumnBinder t m v r a = ReaderT (TreeBindingSetup m v r) (Moment t) a

bindTreeList :: (TreeViewClass view, Frameworks t)
    => view -- ^ The TreeView to bind
    -> Behavior t [row] -- ^ Row data to display
    -> (forall model. ColumnBinder t model view row a) -- ^ Action to bind individual columns
    -> Moment t a
bindTreeList view rows m = do
    i <- initial rows
    -- NOTE: We don't actually monitor the behavior for changes yet!
    store <- liftIO $ listStoreNew i
    liftIO $ treeViewSetModel view store
    runReaderT m $ TreeBindingSetup view store
data TreeBindingSetup model view row where
    TreeBindingSetup :: (TreeViewClass view,
                         TreeModelClass (model row),
                         TypedTreeModelClass model)
                     => { tbsView :: view
                        , tbsModel :: model row 
                        } 
                     -> TreeBindingSetup model view row

bindCol' :: (Frameworks t)
    => CRenderer a
    -> TreeViewColumn 
    -> (row -> a) 
    -> ColumnBinder t m v row ()
bindCol' r col f = ask >>= \setup -> 
    case setup of -- unwrap the GADT to get our typeclass contexts
        (TreeBindingSetup _ model) -> liftIO $ mkRenderer r col f model
    
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

