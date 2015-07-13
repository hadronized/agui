-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.El (
    El(..)
  ) where

import Control.Concurrent.Event ( Event, Trigger, newEvent, trigger )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Graphics.UI.AGUI.Core.Layout ( Layout )
import Graphics.UI.AGUI.Core.Margin ( Margin )
import Graphics.UI.AGUI.Core.Padding ( Padding )
import Graphics.UI.AGUI.Core.Placement ( Placement )
import Graphics.UI.AGUI.Core.Renderer ( Renderer )

-- |The core 'El' type.
data El a = El {
    -- |Element held value.
    elValue     :: a
    -- |Element 'Margin'.
  , elMargin    :: Margin
    -- |Element 'Padding'.
  , elPadding   :: Padding
    -- |Element 'Placement'.
  , elPlacement :: Placement
    -- |Element 'Layout'.
  , elLayout    :: Layout
    -- |Render the element.
  , elRender    :: (a -> IO ()) -> IO ()
  }

instance Functor El where
  fmap f (El a mar pad pla lay rend) =
    El (f a) mar pad pla lay $ \rb -> rend $ rb . f
