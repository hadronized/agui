-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Placement (
    -- * Placement
    Placement(..)
  ) where

import Graphics.UI.AGUI.Core.Metrics ( Pixels )

-- |'Placement' places an element. There’re currently two modes:
--
--   - 'Auto': the element is placed regarding the parent’s 'Placement' ;
--   - @'Relative' x y w h@: the element is placed at @(x,y)@ and has size
--     @(w,h)@.
--
-- For the 'Relative' mode, only 'x' and 'y' are relative. 'w' and 'h' are not.
-- @x = 0@ and @y = 0@ places the object at the top-left corner of the parent
-- element.
data Placement
  = Auto
  | Relative Pixels Pixels Pixels Pixels
    deriving (Eq,Show)
