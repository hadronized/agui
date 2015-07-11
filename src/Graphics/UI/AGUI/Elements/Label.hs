-----------------------------------------------------------------------------
-- |
-- Copyright   : (C) 2015 Dimitri Sabadie
-- License     : BSD3
--
-- Maintainer  : Dimitri Sabadie <dimitri.sabadie@gmail.com>
-- Stability   : experimental
-- Portability : portable
-----------------------------------------------------------------------------

module Graphics.UI.AGUI.Elements.Label (
    -- *
  ) where

import Control.Monad.IO.Class ( MonadIO )
import Data.String ( IsString )
import Graphics.UI.AGUI.Core.El ( BuildEl, El, newEl )

newtype Label = Label { unLabel :: String } deriving (Eq,IsString,Ord,Show)

newLabel :: (MonadIO m) => String -> BuildEl m Label
newLabel = newEl . Label
