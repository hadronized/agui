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

newtype Widget a = Widget { unWidget :: IO (El a) } deriving (Functor)

newWidget :: (MonadIO m)
          => a
          -> Margin
          -> Padding
          -> Placement
          -> Layout
          -> ((a -> IO ()) -> IO ())
          -> m (Widget a,Event (El a),Trigger (El a))
newWidget a mar pad pla lay rend = do
  ref <- liftIO . newIORef $ El a mar pad pla lay rend
  (e,t) <- newEvent
  pure (Widget (readIORef ref),e,Trigger (writeIORef ref) <> t)
