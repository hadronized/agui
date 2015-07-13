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

import Control.Concurrent.Event ( Event, Trigger, newEvent, trigger )
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
--
-- If you change the type of a 'Widget', you cannot
newtype Widget a = Widget { unWidget :: IO (El a) } deriving (Functor)

newWidget :: (MonadIO m)
          => a
          -> Margin
          -> Padding
          -> Placement
          -> Layout
          -> ((a -> IO ()) -> IO ())
          -> m (Widget a,Trigger (El a))
newWidget a mar pad pla lay rend = do
  (e,t) <- newEvent
  ref <- liftIO . newIORef $ El a mar pad pla lay e rend
  pure (Widget (readIORef ref),Trigger (writeIORef ref) <> t)

