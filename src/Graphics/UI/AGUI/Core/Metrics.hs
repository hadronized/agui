-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Core.Metrics (
    -- * Pixels
    Pixels(..)
  , px
    -- * Percent
  , Percent(..)
  , percent
  ) where

newtype Pixels = Pixels { unPixels :: Float }
  deriving (Eq,Floating,Fractional,Num,Ord,Show)

px :: Float -> Pixels
px = Pixels

newtype Percent = Percent { unPercent :: Float }
  deriving (Eq,Floating,Fractional,Num,Ord,Show)

percent :: Float -> Percent
percent = Percent
