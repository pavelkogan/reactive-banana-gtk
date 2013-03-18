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
import Control.Error

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
    changeButton <- builderGetObject builder castToButton "changeItem"
    
    newItemName <- builderGetObject builder castToEntry "newItemName"
    
    outputLabel <- builderGetObject builder castToLabel "label"
    
    network <- compile $ do
        nameB <- liftM (stepper "") $ entryText' newItemName
        addTrigger <- event0 addButton buttonActivated
        delTrigger <- event0 delButton  buttonActivated
        changeTrigger <- event0 changeButton  buttonActivated
        
        rec 
            let theList = Map.fromList [(10, "one"), (11, "two")]
                
                newIdE = accumE 11 $ (+ 1) <$ addTrigger
                
                selectedKeyB = liftA fst <$> selection
                selectedValueB = liftA snd <$> selection
                additionE = (flip MapSet) <$> nameB <@> newIdE
                
                deletionB = liftA (MapRemove) <$> selectedKeyB
                deletionE = filterJust $ deletionB <@ delTrigger
                
                changeB = liftA2 MapSet <$> (liftA fst <$> selection) <*> (Just <$> nameB)
                changeE = filterJust $ changeB <@ changeTrigger
                deltas = additionE `union` deletionE `union` changeE    
            (selection, _) <- bindTreeList view theList deltas $ do
                bindTextCol column id
            
        sink outputLabel [labelLabel :== (fromMaybe "") <$> selectedValueB]
        --reactimate $ (putStrLn . show) <$> changes selection
    actuate network
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI