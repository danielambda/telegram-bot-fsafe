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
  , Aboba(..)
  ) where

import Data.Kind (Type, Constraint)
import Telegram.Bot.FSAfe.BotM (BotContext)
import Telegram.Bot.FSAfe.TaggedContext (TaggedContext (EmptyTaggedContext))
import Telegram.Bot.FSAfe.DSL (IsMessage, ProperMessage, Message, Proper')

type IsState :: k -> (Type -> Type) -> Constraint
class IsState a m | a -> m where
  data StateData a :: Type
  parseTransition :: StateData a -> BotContext -> Maybe (SomeTransitionFrom m a)
  type StateMessage a :: Message
  extractMessageContext :: StateData a -> m (Aboba (Proper' (StateMessage a)))
  default extractMessageContext :: (Applicative m, IsMessage (Proper' (StateMessage a)) '[])
                                => StateData a -> m (Aboba (Proper' (StateMessage a)))
  extractMessageContext _ = pure $ Aboba EmptyTaggedContext

type Aboba :: ProperMessage -> Type
data Aboba a where
  Aboba :: IsMessage a ctx => TaggedContext ctx -> Aboba a

type SomeStateData :: (Type -> Type) -> Type
data SomeStateData m where
  SomeStateData :: IsState a m => StateData a -> SomeStateData m

type IsTransition :: Type -> k -> (Type -> Type) -> k -> Constraint
class (IsState from m, IsState to m) => IsTransition t from m to | from t -> m, from t -> to where
  handleTransition :: t -> StateData from -> m (StateData to)

type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom m from where
  SomeTransition :: IsTransition t from m to => t -> SomeTransitionFrom m from

