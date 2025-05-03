{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  , MessageContext(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, ProperMessageKind, MessageKind, Proper')
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)

type IsState :: k -> (Type -> Type) -> Constraint
class IsState a m | a -> m where
  data StateData a :: Type
  parseTransition :: StateData a -> BotContext -> Maybe (SomeTransitionFrom m a)
  type StateMessage a :: MessageKind
  extractMessageContext :: StateData a -> m (MessageContext (Proper' (StateMessage a)))
  -- TODO make this default more generic via creating help typeclass,
  -- there should be 2 default cases: empty and when fields of StateData are sufficient
  default extractMessageContext :: (Applicative m, IsMessage (Proper' (StateMessage a)) '[])
                                => StateData a -> m (MessageContext (Proper' (StateMessage a)))
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: ProperMessageKind -> Type
data MessageContext a where
  MessageContext :: IsMessage a ctx => TaggedContext ctx -> MessageContext a

type SomeStateData :: (Type -> Type) -> Type
data SomeStateData m where
  SomeStateData :: IsState a m => StateData a -> SomeStateData m

type IsTransition :: Type -> k -> (Type -> Type) -> k -> Constraint
class (IsState from m, IsState to m) => IsTransition t from m to | from t -> m, from t -> to where
  handleTransition :: t -> StateData from -> m (StateData to)

type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom m from where
  SomeTransition :: IsTransition t from m to => t -> SomeTransitionFrom m from

