module Telegram.Bot.FSAfe.FSA.StateMessage where

import Telegram.Bot.FSAfe.Message (MessageShowMode)

class StateMessageM m a where
  stateMessageM :: a -> m MessageShowMode

instance {-# OVERLAPPABLE #-}
         (Applicative m, StateMessage a) => StateMessageM m a where
  stateMessageM = pure . stateMessage

class StateMessage a where
  stateMessage :: a -> MessageShowMode
