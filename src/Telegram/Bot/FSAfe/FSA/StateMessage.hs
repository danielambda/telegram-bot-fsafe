module Telegram.Bot.FSAfe.FSA.StateMessage where

import Telegram.Bot.FSAfe.Message (ShowMode)

class StateMessageM m a where
  stateMessageM :: a -> m ShowMode

instance {-# OVERLAPPABLE #-}
         (Applicative m, StateMessage a) => StateMessageM m a where
  stateMessageM = pure . stateMessage

class StateMessage a where
  stateMessage :: a -> ShowMode
