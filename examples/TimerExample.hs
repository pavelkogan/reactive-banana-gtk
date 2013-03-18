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
    
incrementFrom :: a -> State Int ()
incrementFrom = const $ do
    v <- get
    put $ v + 1
    
test :: IO ()
test = do
    initGUI
    window <- windowNew
    label <- labelNew Nothing
    containerAdd window label
    
    network <- compile $ do
        ticks <- timer 500
        let nums = (liftA show . snd) $ runStateReactive incrementFrom 0 ticks
        sink label [labelLabel :== nums]
    
    actuate network
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI