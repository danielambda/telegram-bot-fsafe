{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..),      SomeStateData(..)
  , IsTransition(..), SomeTransitionFrom(..)
  , MessageContext(..)
  , parseSomeTransitionFromState, Aboba(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Data.Data (Proxy (..))
import Control.Applicative ((<|>))

class Aboba a m ts | a -> m where
  parseSomeTransition :: Proxy ts -> a -> BotContext -> Maybe (SomeTransitionFrom m a)

instance IsState a m => Aboba a m '[] where
  parseSomeTransition _ _ _ = Nothing

instance (IsTransition t a m b, Aboba a m ts) => Aboba a m (t ': ts) where
  parseSomeTransition _ a botCtx
    =   SomeTransition <$> parseTransition @t a botCtx
    <|> parseSomeTransition (Proxy @ts) a botCtx

parseSomeTransitionFromState
  :: forall a m. Aboba a m (OutgoingTransitions a)
  => a -> BotContext -> Maybe (SomeTransitionFrom m a)
parseSomeTransitionFromState = parseSomeTransition $ Proxy @(OutgoingTransitions a)

type IsState :: Type -> (Type -> Type) -> Constraint
class IsState a m | a -> m where
  type OutgoingTransitions a :: [Type]
  type StateMessage a :: MessageKind

  extractMessageContext :: a -> m (MessageContext a)
  default extractMessageContext ::
    ( Applicative m
    , IsMessage (Proper' (StateMessage a)) ctx
    , HasTaggedContext ctx a
    ) => a -> m (MessageContext a)
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: k -> Type
data MessageContext a where
  MessageContext
    :: ( IsMessage (Proper' (StateMessage a)) ctx
       , HasTaggedContext ctx0 a
       , ctx ~ ctx1 ++ ctx0
       )
    => TaggedContext ctx1 -> MessageContext a

type SomeStateData :: (Type -> Type) -> Type
data SomeStateData m where
  SomeStateData :: (Aboba a m (OutgoingTransitions a), IsState a m) => a -> SomeStateData m

type IsTransition :: Type -> Type -> (Type -> Type) -> Type -> Constraint
class (IsState from m, Aboba to m (OutgoingTransitions to), IsState to m) => IsTransition t from m to | from t -> m, from t -> to where
  parseTransition :: from -> BotContext -> Maybe t
  handleTransition :: t -> from -> m to

type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom m from where
  SomeTransition :: IsTransition t from m to => t -> SomeTransitionFrom m from

