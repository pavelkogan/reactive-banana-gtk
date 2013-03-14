module TreeListExample where
import Graphics.UI.Gtk
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk.Collections
 
test = do
    initGUI
    builder <- builderNew
    builderAddFromFile builder "examples/TreeListExample.glade"
    window <- builderGetObject builder castToWindow "mainWindow"
    
    view <- builderGetObject builder castToTreeView "treeview"
    column <- treeViewColumnNew
    treeViewAppendColumn view column
    
    network <- compile $ do
        let theList = pure ["One", "Two", "Three"]
        bindTreeList view theList $ do
            bindTextCol column id
    actuate network
    
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI