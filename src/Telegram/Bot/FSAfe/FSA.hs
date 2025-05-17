{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..), SomeState(..)
  , SomeTransitionFrom(..)
  , MessageContext(..)
  , HasState(..)
  , HList(..), HList'(..)
  , Transition(..)
  , Extract'(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext, BotM)
import Control.Applicative ((<|>))
import Data.Proxy (Proxy(..))

class Extract' s fsa ts | s fsa -> ts where
  extract :: Proxy s -> HList' fsa -> HList ts

instance Extract' s '[] '[] where
  extract _ HNil' = HNil

instance {-# OVERLAPPING #-} Extract' s ('(s, ts) ': fsa) ts where
  extract _ (HCons' _ ts _) = ts

instance {-# OVERLAPPABLE #-} Extract' s fsa ts => Extract' s ('(x, xts) ': fsa) ts where
  extract s (HCons' _ _ fsa) = extract s fsa

data HList ts where
  HNil :: HList '[]
  HCons :: t -> HList ts -> HList (t ': ts)

type HList' :: [(Type, [Type])] -> Type
data HList' fsa where
  HNil' :: HList' '[]
  HCons' :: Proxy a -> HList t -> HList' ts -> HList' ('(a, t) ': ts)

class Extract' s fsa ts => HasState s fsa ts where
  parseSomeTransition
    :: HList ts -> s -> BotContext -> Maybe (SomeTransitionFrom s fsa)

instance (Extract' s fsa ts, HasState' s ts fsa) => HasState s fsa ts where
  parseSomeTransition = parseSomeTransition'

type HasState' :: Type -> [Type] -> [(Type, [Type])] -> Constraint
class HasState' s ts fsa where
  parseSomeTransition' :: HList ts -> s -> BotContext -> Maybe (SomeTransitionFrom s fsa)

instance HasState' s '[] fsa where
  parseSomeTransition' _ _ _ = Nothing

instance (IsState s, IsState s', HasState' s ts fsa, HasState s' fsa ts')
      => HasState' s (Transition t s s' ': ts) fsa where
  parseSomeTransition' (HCons Transition{..} ts) s botCtx
    =   SomeTransition Transition{..} <$> parseTransition s botCtx
    <|> parseSomeTransition' ts s botCtx

type IsState :: Type -> Constraint
class IsState a where
  type StateMessage a :: MessageKind

  extractMessageContext :: a -> BotM (MessageContext a)
  default extractMessageContext ::
    ( -- Applicative m
     IsMessage (Proper' (StateMessage a)) ctx
    , HasTaggedContext ctx a
    ) => a -> BotM (MessageContext a)
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: k -> Type
data MessageContext a where
  MessageContext
    :: ( IsMessage (Proper' (StateMessage a)) ctx
       , HasTaggedContext ctx0 a
       , ctx ~ ctx1 ++ ctx0
       )
    => TaggedContext ctx1 -> MessageContext a

type SomeState :: [(Type, [Type])] -> Type
data SomeState fsa where
  SomeState :: (Extract' s fsa ts, HasState s fsa ts) => HList' fsa -> s -> SomeState fsa

data Transition t from to = Transition
  { parseTransition :: from -> BotContext -> Maybe t
  , handleTransition :: t -> from -> to
  }

-- type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom from fsa where
  SomeTransition :: (Extract' to fsa ts, HasState to fsa ts, IsState to, IsState from)
                 => Transition t from to -> t -> SomeTransitionFrom from fsa

