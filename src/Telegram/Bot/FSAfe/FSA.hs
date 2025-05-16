{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

-- module Telegram.Bot.FSAfe.FSA
--   ( IsState(..),      SomeStateData(..)
--   , IsTransition(..), SomeTransitionFrom(..)
--   , MessageContext(..)
--   , parseSomeTransitionFromState, Aboba(..)
--   ) where
module Telegram.Bot.FSAfe.FSA where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Control.Applicative ((<|>))
import Data.Proxy (Proxy)

type Extract :: Type -> [(Type, [Type])] -> [Type]
type family Extract state fsa where
  Extract _ '[] = '[]
  Extract a ('(a, ts) ': fsa) = ts
  Extract a ('(b, ts) ': fsa) = Extract a fsa

data HList ts where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

class Aboba a m (Extract a fsa) => Arbuz a m fsa where
  parseSomething :: Proxy a -> Proxy fsa -> HList (Extract a fsa) -> a -> BotContext -> Maybe (SomeTransitionFrom m a)

class Aboba a m ts | a -> m where
  parseSomeTransition :: HList ts -> a -> BotContext -> Maybe (SomeTransitionFrom m a)

instance IsState a m => Aboba a m '[] where
  parseSomeTransition _ _ _ = Nothing

instance (IsState from m, IsState to m, Aboba from m ts)
      => Aboba from m (Transition t from to m ': ts) where
  parseSomeTransition (HCons Transition{..} ts) from botCtx
    =   SomeTransition Transition{..} <$> parseTransition from botCtx
    <|> parseSomeTransition ts from botCtx

type IsState :: Type -> (Type -> Type) -> Constraint
class IsState a m | a -> m where
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
  SomeStateData :: (Aboba a m ts, IsState a m) => HList ts -> a -> SomeStateData m

data Transition t from to m = Transition
  { parseTransition :: from -> BotContext -> Maybe t
  , handleTransition :: t -> from -> m to
  }

-- type IsTransition :: Type -> Type -> (Type -> Type) -> Type -> Constraint
-- class (IsState from m, Aboba to m (OutgoingTransitions to), IsState to m) => IsTransition t from m to | from t -> m, from t -> to where
--   parseTransition :: from -> BotContext -> Maybe t
--   handleTransition :: t -> from -> m to

-- type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom m from where
  SomeTransition :: (IsState from m, IsState to m)
                 => Transition t from to m -> t -> SomeTransitionFrom m from

