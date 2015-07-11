-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Padding (
    -- * Paddings
    Padding(..)
  , padding
  ) where

import Graphics.UI.AGUI.Core.Metrics ( Pixels )
import Graphics.UI.AGUI.Core.Spacing ( Spacing(..) )

-- |'Padding' is used to describe the spaces between an element and inner
-- elements.
newtype Padding = Padding { paddingSpacing :: Spacing } deriving (Eq,Show)

-- |Build a 'Padding'.
padding :: Pixels -> Pixels -> Pixels -> Pixels -> Padding
padding t r b l = Padding $ Spacing t r b l
