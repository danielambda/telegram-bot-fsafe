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

import Telegram.Bot.FSAfe.BotM (BotContext)
import Control.Applicative ((<|>))
import Data.Proxy (Proxy(..))
import Telegram.Bot.FSAfe.BotContextParser (BotContextParser, runBotContextParser)

class Extract' s fsa ts | s fsa -> ts where
  extract :: Proxy s -> HList' fsa m -> HList ts m

instance Extract' s '[] '[] where
  extract _ HNil' = HNil

instance {-# OVERLAPPING #-} Extract' s ('(s, ts) ': fsa) ts where
  extract _ (HCons' ts _) = ts

instance {-# OVERLAPPABLE #-} Extract' s fsa ts => Extract' s ('(x, xts) ': fsa) ts where
  extract s (HCons' _ fsa) = extract s fsa

type HList :: [Type] -> (Type -> Type) -> Type
data HList ts m where
  HNil :: HList '[] m
  HCons :: Transition t s s' m -> HList ts m -> HList (Transition t s s' m ': ts) m

type HList' :: [(Type, [Type])] -> (Type -> Type) -> Type
data HList' fsa m where
  HNil' :: HList' '[] m
  HCons' :: forall a t ts m. HList t m -> HList' ts m -> HList' ('(a, t) ': ts) m

class Extract' s fsa ts => HasState s fsa ts where
  parseSomeTransition :: HList ts m -> s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance (Extract' s fsa ts, HasState' s ts fsa) => HasState s fsa ts where
  parseSomeTransition = parseSomeTransition'

type HasState' :: Type -> [Type] -> [(Type, [Type])] -> Constraint
class HasState' s ts fsa where
  parseSomeTransition' :: HList ts m -> s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance HasState' s '[] fsa where
  parseSomeTransition' _ _ _ = Nothing

instance (IsState s m, IsState s' m, HasState' s ts fsa, HasState s' fsa ts')
      => HasState' s (Transition t s s' m ': ts) fsa where
  parseSomeTransition' (HCons Transition{..} ts) s botCtx
    =   SomeTransition Transition{..} <$> runBotContextParser (parseTransition s) botCtx
    <|> parseSomeTransition' ts s botCtx

type IsState :: Type -> (Type -> Type) -> Constraint
class IsState a m where
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

type SomeState :: [(Type, [Type])] -> (Type -> Type) -> Type
data SomeState fsa m where
  SomeState :: (Extract' s fsa ts, HasState s fsa ts) => HList' fsa m -> s -> SomeState fsa m

data Transition t from to m = Transition
  { parseTransition :: from -> BotContextParser t
  , handleTransition :: t -> from -> m to
  }

-- type SomeTransitionFrom :: (Type -> Type) -> k -> Type
data SomeTransitionFrom from fsa m where
  SomeTransition :: (Extract' to fsa ts, HasState to fsa ts, IsState to m, IsState from m)
                 => Transition t from to m -> t -> SomeTransitionFrom from fsa m

