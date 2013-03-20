{-# LANGUAGE ExistentialQuantification, RankNTypes #-}
module Reactive.Banana.Gtk (
  AttrBinding(..), {- eventM, -} event0, event1, event2, event3,
  monitorAttr, pollAttr, sink
) where

import Reactive.Banana
import Reactive.Banana.Frameworks
import System.Glib.Attributes (ReadWriteAttr, AttrOp((:=)))
import qualified System.Glib.Attributes as Attrs
import System.Glib.Signals (Signal, on, signalDisconnect)

import qualified Graphics.UI.Gtk as Gtk

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

-- | Bind a nullary GTK signal, e.g. 'buttonPress', as an @Event t ()@
event0 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (IO ())
    -> Moment t (Event t ())
event0 = eventN ($ ())

-- | Bind a GTK signal that contains one value
event1 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> IO ())
    -> Moment t (Event t a)
event1 = eventN id

-- | Bind a GTK signal that contains two values
event2 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> b -> IO ())
    -> Moment t (Event t (a, b))
event2 = eventN curry

-- | Bind a GTK signal that contains three values
event3 :: (Frameworks t, Gtk.GObjectClass self) 
    => self
    -> Signal self (a -> b -> c -> IO ())
    -> Moment t (Event t (a, b, c))
event3 = eventN $ \f a b c -> f (a, b, c)

-- | Create an Event that occurs whenever a certain 'Signal' triggers, but containing
--   the current value of a (potentially-unrelated) 'Gtk.Attr'.  Useful, e.g., for widgets like
--   'Gtk.Entry', where the update signal ('Gtk.editableChanged') does not contain the new
--   value, which is instead in 'Gtk.entryText'.
monitorAttr :: 
    (Frameworks t, Gtk.GObjectClass self)
    => self
    -> Signal self (IO ()) -- ^ Signal indicating when to read the attribute
    -> ReadWriteAttr self a b
    -> Moment t (Event t a)
monitorAttr self signal attr =
    fromAddHandler $ \e -> do
        callbackId <- on self signal $ Attrs.get self attr >>= e >> return ()
        return $ signalDisconnect callbackId

-- | Turn an 'Gtk.Attr' into an 'Event' by polling.  Avoid using this.
pollAttr :: (Frameworks t) => self -> ReadWriteAttr self a b -> Moment t (Behavior t a)
pollAttr widget attr = fromPoll $ liftIO $ Attrs.get widget attr

data AttrBinding t o = forall a b. (ReadWriteAttr o a b) :== Behavior t b

infixr 0 :==

sink :: (Frameworks t) => self -> [AttrBinding t self] -> Moment t ()
sink self = mapM_ sink'
  where
    sink' (attr :== x) = do
      i <- initial x
      xs <- changes x
      liftIOLater $ Attrs.set self [attr := i]
      reactimate $ (\x -> Attrs.set self [attr := x]) <$> xs

-- reactimateEventM :: Event (Ptr a) -> Gtk.EventM a () -> Moment ()
-- reactimateEventM event reader = reactimate $ (<$> event) $ runReaderT reader
