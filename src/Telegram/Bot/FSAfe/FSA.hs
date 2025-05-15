{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  , MessageContext(..)
  , HasState(..), StateTransitions(..)
  , FSA(..), FSAMonad
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Control.Applicative ((<|>))

infix 4 :@
data FSA = [StateTransitions] :@ (Type -> Type)
infix 4 :~
data StateTransitions = Type :~ [Type]

type FSAMonad :: FSA -> (Type -> Type)
type family FSAMonad fsa where
  FSAMonad (_ :@ m) = m

type HasState :: FSA -> Type -> Constraint
class HasState fsa state where
  parseTransitions :: state -> BotContext -> Maybe (SomeTransitionFrom state (FSAMonad fsa))

instance {-# OVERLAPPABLE #-}
         HasState ((state :~ '[]) ': ts :@ m) state where
  parseTransitions _ _ = Nothing

instance {-# OVERLAPPABLE #-}
         HasState (fsa :@ m) state
      => HasState (ts ': fsa :@ m) state where
  parseTransitions = parseTransitions @(fsa :@ m)

instance {-# OVERLAPPING #-}
         ( IsTransition t state to m
         , HasState ((state :~ ts) ': ts' :@ m) state
         )
      => HasState ((state :~ (t ': ts)) ': ts' :@ m) state where
  parseTransitions state botCtx
    =   SomeTransition <$> parseTransition @t @state @to @m state botCtx
    <|> parseTransitions @((state :~ ts) ': ts' :@ m) state botCtx

type IsState :: Type -> Constraint
class IsState s where
  type StateMessage s :: MessageKind
  extractMessageContext :: s -> m (MessageContext s) -- TODO make extracting from monad
  -- some typeclass using instance, then this method maybe will be removed
  default extractMessageContext ::
    ( Applicative m
    , IsMessage (Proper' (StateMessage s)) ctx
    , HasTaggedContext ctx s
    ) => s -> m (MessageContext s)
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: Type -> Type
data MessageContext a where
  MessageContext
    :: ( IsMessage (Proper' (StateMessage a)) ctx
       , HasTaggedContext ctx0 a
       , ctx ~ ctx1 ++ ctx0
       )
    => TaggedContext ctx1 -> MessageContext a

type SomeStateData :: FSA -> Type
data SomeStateData fsa where
  SomeStateData :: (IsState s, HasState fsa s) => s -> SomeStateData fsa

type IsTransition :: Type -> Type -> Type -> (Type -> Type) -> Constraint
class (IsState from, IsState to) => IsTransition t from to m | from t -> to where
  parseTransition :: from -> BotContext -> Maybe t
  handleTransition :: t -> from -> m to

type SomeTransitionFrom :: Type -> (Type -> Type) -> Type
data SomeTransitionFrom from m where
  SomeTransition :: IsTransition t from to m => t -> SomeTransitionFrom from m
