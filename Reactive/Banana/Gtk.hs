{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Reactive.Banana.Gtk (
  AttrBinding(..), {- eventM, -} event0, event1, event2, event3,
  monitorAttr, monitorF, pollAttr, sink,
  intervals, intervalsWithControl, IntervalControl(..)
) where

import Data.IORef

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Glib.Attributes (ReadWriteAttr, AttrOp((:=)))
import qualified System.Glib.Attributes as Attrs
import System.Glib.Signals (Signal, on, signalDisconnect)

import qualified Graphics.UI.Gtk as Gtk

-- | Emit periodic events using a GTK timer.  The timer
--   (probably?) begins immediately when the EventNetwork is 'compile'd.
intervals :: (Frameworks t)
    => Int -- ^ Time, in msec, between event firings
    -> Moment t (Event t ())
intervals period = fromAddHandler $ \rbCallback -> do
    callbackId <- Gtk.timeoutAdd 
        (rbCallback () >> return True) -- don't stop firing until removed
        period
    return $ Gtk.timeoutRemove callbackId

data IntervalControl = StartIntervals | StopIntervals
-- | Emit periodic events using a GTK timer.  This timer may be started and stopped
--   with an input Event.  It does not begin until it receives a StartTimer input.
--   If it receives a StartTimer while the timer is already running, the current
--   timer is restarted.
intervalsWithControl :: (Frameworks t)
    => Event t IntervalControl -- ^ Controller to start or stop timer
    -> Int -- ^ Time, in msec, between event firings
    -> Moment t (Event t ())
intervalsWithControl control period = do
    banananHandlerRef <- liftIO $ newIORef undefined
    timerRef <- liftIO $ newIORef Nothing
    let f StartIntervals = do
            f StopIntervals  -- Any concern regarding race conditions?
            newTimer <- (flip Gtk.timeoutAdd) period $ do
                bananaHandler <- readIORef banananHandlerRef
                bananaHandler ()
                return True
            writeIORef timerRef $ Just newTimer
        f StopIntervals = do
            currentTimer <- readIORef timerRef
            case currentTimer of
                Nothing -> return ()
                Just t -> Gtk.timeoutRemove t >> writeIORef timerRef Nothing
    reactimate $ f <$> control 
    -- I hope there's no chance of running f before the handler is registered.
    fromAddHandler $ \bananaHandler -> do
        liftIO $ writeIORef banananHandlerRef bananaHandler
        return $ f StopIntervals

{-
eventM :: (Frameworks t, Gtk.GObjectClass self)
    => self
    -> Signal self (Gtk.EventM a Bool)
    -> Gtk.EventM a b
    -> Moment t (Event t b)
eventM self signal m = 
    fromAddHandler $ \e -> do
        callbackId <- on self signal $ m >> return False -- e is not used here!
        return $ signalDisconnect callbackId
-}
eventN :: (Frameworks t, Gtk.GObjectClass self) 
    => ((a -> IO ()) -> callback) 
    -> self
    -> Signal self callback
    -> Moment t (Event t a)
eventN f self signal =
    fromAddHandler $ \e -> do
        let 
            callback = f e
        callbackId <- on self signal callback
        return $ signalDisconnect callbackId

event0 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (IO ())
    -> Moment t (Event t ())
event0 = eventN ($ ())

event1 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> IO ())
    -> Moment t (Event t a)
event1 = eventN id

event2 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> b -> IO ())
    -> Moment t (Event t (a, b))
event2 = eventN curry

event3 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> b -> c -> IO ())
    -> Moment t (Event t (a, b, c))
event3 = eventN $ \f a b c -> f (a, b, c)

monitorAttr :: (Frameworks t, Gtk.GObjectClass self)
    => self
    -> Signal self (IO ()) -- ^ Signal indicating when to read the attribute
    -> ReadWriteAttr self a b
    -> Moment t (Event t a)
monitorAttr self signal attr =
    fromAddHandler $ \e -> do
        callbackId <- on self signal $ Attrs.get self attr >>= e >> return ()
        return $ signalDisconnect callbackId

monitorF :: (Frameworks t, Gtk.GObjectClass self)
    => self
    -> Signal self (IO ()) -- ^ Signal indicating when to read the attribute
    -> (self -> IO a)
    -> Moment t (Event t a)
monitorF self signal f =
    fromAddHandler $ \e -> do
        callbackId <- on self signal $ f self >>= e >> return ()
        return $ signalDisconnect callbackId
pollAttr :: (Frameworks t) => self -> ReadWriteAttr self a b -> Moment t (Behavior t a)
pollAttr widget attr = fromPoll $ liftIO $ Attrs.get widget attr

data AttrBinding t o = forall a b. (ReadWriteAttr o a b) :== Behavior t b

infixr 0 :==

sink :: (Frameworks t) => self -> [AttrBinding t self] -> Moment t ()
sink self = mapM_ sink'
  where
    sink' (attr :== xB) = do
      i <- initial xB
      xE <- changes xB
      liftIOLater $ Attrs.set self [attr := i]
      reactimate $ (\x -> Attrs.set self [attr := x]) <$> xE

-- reactimateEventM :: Event (Ptr a) -> Gtk.EventM a () -> Moment ()
-- reactimateEventM event reader = reactimate $ (<$> event) $ runReaderT reader
