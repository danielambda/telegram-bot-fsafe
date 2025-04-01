{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}

module Telegram.Bot.FSAfe.FSA
  ( FSAfeM
  , IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  ) where


import Data.Kind (Type, Constraint)
import Telegram.Bot.FSAfe.BotM (BotContext)

type family FSAfeM :: Type -> Type

type IsState :: k -> Constraint
class IsState a where
  data StateData a :: Type
  parseTransition :: StateData a -> BotContext -> Maybe (SomeTransitionFrom a)

data SomeStateData where
  SomeStateData :: IsState a => StateData a -> SomeStateData

type IsTransition :: Type -> k -> k -> Constraint
class (IsState from, IsState to) => IsTransition t from to | from t -> to where
  handleTransition :: t -> StateData from -> FSAfeM (StateData to)

type SomeTransitionFrom :: k -> Type
data SomeTransitionFrom from where
  SomeTransition :: IsTransition t from to => t -> SomeTransitionFrom from

