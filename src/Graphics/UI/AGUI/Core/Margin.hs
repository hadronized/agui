-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Margin (
    -- * Margns
    Margin(..)
  , margin
  ) where

import Graphics.UI.AGUI.Core.Metrics ( Pixels )
import Graphics.UI.AGUI.Core.Spacing ( Spacing(..) )

-- |'Margin' is used to describe the spaces between an element and outer
-- elements.
newtype Margin = Margin { marginSpacing :: Spacing } deriving (Eq,Show)

-- |Build a 'Margin'.
margin :: Pixels -> Pixels -> Pixels -> Pixels -> Margin
margin t r b l = Margin $ Spacing t r b l
