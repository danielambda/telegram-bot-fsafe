{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.FSAfe.FSA where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Data.Proxy (Proxy (..))

type Transition :: Type -> Type -> Type
data Transition a t

infixl 5 :>-
type (:>-) :: Type -> Type -> Type
type a :>- t = Transition a t

infixl 5 :->
type (:->) :: Type -> Type -> Type
type at :-> b = at

type SomeStateFrom :: [Type] -> (Type -> Type) -> Type
data SomeStateFrom fsa m where
  SomeStateFrom :: (IsState state m, HasState state fsa m) => state -> SomeStateFrom fsa m

type HasState :: Type -> [Type] -> (Type -> Type) -> Constraint
class HasState state fsa m | fsa -> m where
  handleState :: BotContext -> state -> m (SomeStateFrom fsa m)

instance Aboba fsa state (TransitionsOfState state fsa) m => HasState state fsa m where
  handleState = handleAboba (Proxy @(TransitionsOfState state fsa))

type family TransitionsOfState a fsa where
  TransitionsOfState _ '[] = '[]
  TransitionsOfState a (Transition a t ': fsa) = t ': TransitionsOfState a fsa
  TransitionsOfState a (_ ': fsa) = TransitionsOfState a fsa

type Aboba :: [Type] -> Type -> [Type] -> (Type -> Type) -> Constraint
class Aboba fsa state transitions m | fsa -> m where
  handleAboba :: Proxy transitions -> BotContext -> state -> m (SomeStateFrom fsa m)

instance (Applicative m, HasState state fsa m, IsState state m)
      => Aboba fsa state '[] m where
  handleAboba _ _ state = pure $ SomeStateFrom state

instance (Functor m, Aboba fsa state ts m, IsTransition t state b m, HasState b fsa m)
      => Aboba fsa state (t ': ts) m where
  handleAboba _ botCtx state =
    case parseTransition @t state botCtx of
      Nothing -> handleAboba (Proxy @ts) botCtx state
      Just t -> SomeStateFrom <$> handleTransition state t

class IsState to m => IsTransition t from to m | t from -> to, t from -> m where
  parseTransition :: from -> BotContext -> Maybe t
  handleTransition :: from -> t -> m to

type IsState :: Type -> (Type -> Type) -> Constraint
class IsState s m | s -> m where
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
