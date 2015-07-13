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

import Control.Concurrent.Event ( Event )
import Graphics.UI.AGUI.Core.Layout ( Layout )
import Graphics.UI.AGUI.Core.Margin ( Margin )
import Graphics.UI.AGUI.Core.Padding ( Padding )
import Graphics.UI.AGUI.Core.Placement ( Placement )

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
    -- Element 'Event'.
  , elEvent     :: Event (El a)
  }

instance Functor El where
  fmap f (El a mar pad pla lay e) =
    El (f a) mar pad pla lay (fmap (fmap f) e)
