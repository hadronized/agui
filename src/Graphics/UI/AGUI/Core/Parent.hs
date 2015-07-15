-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Parent (
    -- * Parent
    Parent(..)
  ) where

import Graphics.UI.AGUI.Core.El ( El )
import Graphics.UI.AGUI.Core.Layout ( Layout )

-- |A @'Parent' a@ is an object used to gather several @'El' a@ under a
-- 'Layout'.
data Parent a = Parent {
    -- |Extract the 'Layout'.
    parentLayout :: Layout
    -- |Children held.
  , children :: [El a]
}
