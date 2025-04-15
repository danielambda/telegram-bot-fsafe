{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds #-}

module Telegram.Bot.FSAfe.FSA
  ( FSAfeM
  , IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  , Aboba(..)
  ) where

import Data.Kind (Type, Constraint)
import Telegram.Bot.FSAfe.BotM (BotContext)
import Telegram.Bot.FSAfe.TaggedContext (TaggedContext (EmptyTaggedContext))
import Telegram.Bot.FSAfe.DSL (IsMessage, ProperMessage, Message, Proper')

type family FSAfeM :: Type -> Type

type IsState :: k -> Constraint
class IsState a where
  data StateData a :: Type
  parseTransition :: StateData a -> BotContext -> Maybe (SomeTransitionFrom a)
  type StateMessage a :: Message
  toMessageData :: StateData a -> FSAfeM (Aboba (Proper' (StateMessage a)))
  default toMessageData :: (IsMessage (Proper' (StateMessage a)) '[], Monad FSAfeM)
                        => StateData a -> FSAfeM (Aboba (Proper' (StateMessage a)))
  toMessageData _ = return $ Aboba EmptyTaggedContext

type Aboba :: ProperMessage -> Type
data Aboba a where
  Aboba :: IsMessage a ctx => TaggedContext ctx -> Aboba a

data SomeStateData where
  SomeStateData :: IsState a => StateData a -> SomeStateData

type IsTransition :: Type -> k -> k -> Constraint
class (IsState from, IsState to) => IsTransition t from to | from t -> to where
  handleTransition :: t -> StateData from -> FSAfeM (StateData to)

type SomeTransitionFrom :: k -> Type
data SomeTransitionFrom from where
  SomeTransition :: IsTransition t from to => t -> SomeTransitionFrom from

