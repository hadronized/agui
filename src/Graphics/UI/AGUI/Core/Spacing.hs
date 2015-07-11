-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Spacing (
    -- * Spacing
    Spacing(..)
  ) where

import Graphics.UI.AGUI.Core.Metrics ( Pixels )

-- |A 'Spacing' is an object that describes spaces between objects. It can have
-- two purposes:
--
--   - describe spaces between an element and outer elements ('Margin') ;
--   - describe spaces between an element and inner elements ('Padding').
data Spacing = Spacing {
    -- |Describe the spacing at the top.
    topSpacing :: Pixels
    -- |Describe the spacing at the right.
  , rightSpacing :: Pixels
    -- |Describe the spacing at the bottom.
  , bottomSpacing :: Pixels
    -- |Describe the spacing at the left.
  , leftSpacing :: Pixels
  } deriving (Eq,Show)
