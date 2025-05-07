{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  , MessageContext(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)

type IsState :: k -> (Type -> Type) -> Constraint
class IsState a m | a -> m where
  data StateData a :: Type
  parseTransition :: StateData a -> BotContext -> Maybe (SomeTransitionFrom m a)
  type StateMessage a :: MessageKind
  extractMessageContext :: StateData a -> m (MessageContext a)
  default extractMessageContext ::
    ( Applicative m
    , IsMessage (Proper' (StateMessage a)) ctx
    , HasTaggedContext ctx (StateData a)
    ) => StateData a -> m (MessageContext a)
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: k -> Type
data MessageContext a where
  MessageContext
    :: ( IsMessage (Proper' (StateMessage a)) ctx
       , HasTaggedContext ctx0 (StateData a)
       , ctx ~ ctx1 ++ ctx0
       )
    => TaggedContext ctx1 -> MessageContext a

type SomeStateData :: (Type -> Type) -> Type
data SomeStateData m where
  SomeStateData :: IsState a m => StateData a -> SomeStateData m

type IsTransition :: Type -> k -> (Type -> Type) -> k -> Constraint
class (IsState from m, IsState to m) => IsTransition t from m to | from t -> m, from t -> to where
  handleTransition :: t -> StateData from -> m (StateData to)

type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom m from where
  SomeTransition :: IsTransition t from m to => t -> SomeTransitionFrom m from

