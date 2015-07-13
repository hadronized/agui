----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Widget (
    -- *
  )

import Control.Concurrent.Event ( Event, Trigger, newEvent, on, trigger )
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Graphics.UI.AGUI.Core.El ( El(..) )
import Graphics.UI.AGUI.Core.Layout ( Layout )
import Graphics.UI.AGUI.Core.Margin ( Margin )
import Graphics.UI.AGUI.Core.Padding ( Padding )
import Graphics.UI.AGUI.Core.Placement ( Placement )

-- |A 'Widget' is a /reactive/ object holding information.
--
-- A 'Widget' is a 'Functor', meaning that you can change its type as long as
-- you can still use it later on. For instance, changing the type of a 'Widget'
-- might prevent you from rendering it.
newtype Widget a = Widget { unWidget :: IO (El a) } deriving (Functor)

-- |Create a new 'Widget'.
--
-- You get two objects by creating a new 'Widget'. You get a @'Widget' a@ and a
-- @'Trigger' ('El' a)@. The former represents the 'Widget' itself and can be
-- used to compose the /GUI/. The latter is used to interact with the reactive
-- 'El' value stored in the 'Widget'. You can use the 'Trigger' to change the
-- value of the 'Widget'.
newWidget :: (MonadIO m)
          => a
          -> Margin
          -> Padding
          -> Placement
          -> Layout
          -> ((El a -> IO ()) -> IO ())
          -> m (Widget a,Trigger (El a))
newWidget a mar pad pla lay rend = do
  (e,t) <- newEvent
  ref <- liftIO . newIORef $ El a mar pad pla lay e rend
  pure (Widget (readIORef ref),Trigger (writeIORef ref) <> t)

-- |Register an 'IO' action to be run each time the 'Widget' emits an event.
onW :: (MonadIO m) => Widget a -> (El a -> IO ()) -> m Detach
onW (Widget w) f = liftIO w >>= \el -> on (elEvent el) f
