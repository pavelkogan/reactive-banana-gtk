{-# LANGUAGE DoRec #-}
module TreeListExample where
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk
import Reactive.Banana.Gtk.Collections
import Reactive.Banana.Gtk.MapStore

import Control.Monad (liftM) 
import Data.Map (Map)
import qualified Data.Map as Map

entryText' ::
    (Frameworks t, EditableClass o, EntryClass o)
    => o
    -> Moment t (Event t String)
entryText' self =
    monitorAttr self editableChanged entryText
test = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "examples/TreeListExample.glade"
    window <- builderGetObject builder castToWindow "mainWindow"
    
    view <- builderGetObject builder castToTreeView "treeview"
    column <- builderGetObject builder castToTreeViewColumn "treeviewcolumn1"
    
    addButton <- builderGetObject builder castToButton "addItem"
    delButton <- builderGetObject builder castToButton "deleteItem"
    newItemName <- builderGetObject builder castToEntry "newItemName"
    
    network <- compile $ do
        itemNameE <- entryText' newItemName
        itemAddE <- event0 addButton buttonActivated
        itemDelE <- event0 delButton  buttonActivated
        
        rec 
            let theList = Map.fromList [(10, "one"), (11, "two")]
                itemNameB = stepper "" itemNameE
                newIdE = accumE 11 $ (+ 1) <$ itemAddE
                additionE = (flip MapSet) <$> itemNameB <@> newIdE
                deletionB = liftM MapRemove <$> selection
                deletionE = filterJust $ deletionB <@ itemDelE
                deltas = union additionE deletionE
            (selection, _) <- bindTreeList view theList deltas $ do
                bindTextCol column id
            
        return ()
        --reactimate $ (putStrLn . show) <$> changes selection
    actuate network
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI