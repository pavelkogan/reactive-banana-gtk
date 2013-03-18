module TimerExample where

import Control.Monad.State

import Graphics.UI.Gtk hiding (get)
import Reactive.Banana
import Reactive.Banana.Frameworks
import Reactive.Banana.Gtk


runStateReactive :: (Frameworks t)
    => (a -> State s b) -- ^ Kleisli arrow in in State monad
    -> s 
    -> Event t a
    -> (Event t b, Behavior t s)
runStateReactive stateM s xs =
    mapAccum s $ (runState . stateM) <$> xs
    
incrementReset :: Bool -> State Int ()
incrementReset False = get >>= (put . (+ 1))
incrementReset True = put 0
    
test :: IO ()
test = do
    initGUI
    window <- windowNew
    box <- vBoxNew False 0
    containerAdd window box
    startButton <- buttonNewWithLabel "Start"
    stopButton <- buttonNewWithLabel "Stop"
    resetButton <- buttonNewWithLabel "Reset"
    label <- labelNew Nothing
    mapM (containerAdd box) [startButton, stopButton, resetButton]
    containerAdd box label
    
    network <- compile $ do
        start <- event0 startButton buttonActivated
        stop <- event0 stopButton buttonActivated
        reset <- event0 resetButton buttonActivated
        let control = (StartIntervals <$ start) `union` (StopIntervals <$ stop)
        ticks <- intervalsWithControl control 500
        let ticksWithIncrement = (True <$ reset) `union` (False <$ ticks)
            nums = (liftA show . snd) $ runStateReactive incrementReset 0 ticksWithIncrement
        sink label [labelLabel :== nums]
    
    actuate network
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI