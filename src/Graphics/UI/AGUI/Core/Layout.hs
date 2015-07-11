-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Layout (
    -- * Layout
    Layout(..)
    -- * Layout hints
  , LayoutHints
  , layoutHints
  ) where

import Graphics.UI.AGUI.Core.Metrics ( Percent )

-- |A 'Layout' is used to describe how elements should be placed inside the
-- parent element.
--
-- @'Horizontal' hints@ horizontally splits the parent into several parts. The
-- sizes of those parts is customized via 'hints'. For
-- instance, @'Horizontal' [0.5,0.5]@ splits the parent into two horizontal
-- pieces with same sizes. @'Horizontal' [1]@ and @Horizontal []@ don't split
-- the parent but yield a layout that will cover the whole parent area.
--
-- @'Vertical' hints@ vertically splits the parent into several parts. The sizes
-- of those parts is customized the same way as with 'Horizontal'.
--
-- For both modes, 'hints' has to follow the following rules:
--
-- @ 'sum' hints == 1 @
--
-- If a layout doesn’t fulfil that law, the hints are defaulted to '[]'.
data Layout
  = Horizontal LayoutHints
  | Vertical LayoutHints
    deriving (Eq,Show)

-- |Layout hints.
type LayoutHints = [Percent]

-- |Get the size hints out of a 'Layout'.
layoutHints :: Layout -> LayoutHints
layoutHints l = case l of
  Horizontal h -> h
  Vertical   h -> h
