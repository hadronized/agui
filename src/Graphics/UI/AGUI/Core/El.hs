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
    -- * Element
    El(..)
  , BuildEl
  , newEl
  , (<%>)
  , touch
  ) where

import Control.Concurrent.Event ( Event, Trigger, newEvent, trigger )
import Control.Monad ( void )
import Control.Monad.IO.Class ( MonadIO(..) )
import Graphics.UI.AGUI.Core.Layout ( Layout )
import Graphics.UI.AGUI.Core.Margin ( Margin )
import Graphics.UI.AGUI.Core.Padding ( Padding )
import Graphics.UI.AGUI.Core.Placement ( Placement )
import Graphics.UI.AGUI.Core.Renderer ( Renderer )

-- |'El' is the element tree. It gathers several important types objects:
--
--   - a 'Margin' ;
--   - a 'Padding' ;
--   - a 'Placement' ;
--   - a 'Layout' ;
--   - the held 'a' value ;
--   - a @'Renderer' a@ ;
--   - an @'Event' a@ ;
--   - a @'Trigger a@'.
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
    -- |Element 'Renderer'.
  , elRenderer  :: Renderer a
    -- |Element 'Event'.
  , elEvent     :: Event a
    -- |Element 'Trigger'.
  , elTrigger   :: Trigger a
  }

-- |Convenient type to build specialized 'El'.
type BuildEl m a = Margin -> Padding -> Placement -> Layout -> Renderer a -> m (El a)

-- |Create a new 'El'.
newEl :: (MonadIO m)
      => a
      -> Margin
      -> Padding
      -> Placement
      -> Layout
      -> Renderer a
      -> m (El a)
newEl a mar pad pla lay rend = do
  (e,t) <- newEvent
  pure $ El a mar pad pla lay rend e t

-- |Change the content of an 'El'.
(<%>) :: (MonadIO m) => (a -> a) -> El a -> m (El a)
(<%>) f el = do
    trigger (elTrigger el) a'
    pure $ el { elValue = a' }
  where
    a' = f $ elValue el

-- |Sometimes, it might be useful to act like one has changed an 'El' whilst one
-- has not. That can be used to broadcast events after the creation of an
-- element.
touch :: (MonadIO m) => El a -> m ()
touch = void . (id <%>)
