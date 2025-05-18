{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..), SomeState(..)
  , SomeTransitionFrom(..)
  , MessageContext(..)
  , HasState(..)
  , Extract'
  , IsTransition(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Control.Applicative ((<|>))
import Telegram.Bot.FSAfe.BotContextParser (BotContextParser, runBotContextParser)

type Extract' :: Type -> [(Type, [Type])] -> [Type] -> Constraint
class Extract' s fsa ts | s fsa -> ts where
instance Extract' s '[] '[] where
instance {-# OVERLAPPING #-} Extract' s ('(s, ts) ': fsa) ts where
instance {-# OVERLAPPABLE #-} Extract' s fsa ts => Extract' s ('(x, xts) ': fsa) ts where

type HasState :: Type -> [(Type, [Type])] -> [Type] -> (Type -> Type) -> Constraint
class Extract' s fsa ts => HasState s fsa ts m where
  parseSomeTransition :: s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance (Extract' s fsa ts, HasState' s ts fsa m) => HasState s fsa ts m where
  parseSomeTransition = parseSomeTransition' @s @ts

type HasState' :: Type -> [Type] -> [(Type, [Type])] -> (Type -> Type) -> Constraint
class HasState' s ts fsa m where
  parseSomeTransition' :: s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance HasState' s '[] fsa m where
  parseSomeTransition' _ _ = Nothing

instance (IsTransition t s s' m, Applicative m, IsState s m, IsState s' m, HasState' s ts fsa m, HasState s' fsa ts' m)
      => HasState' s (t ': ts) fsa m where
  parseSomeTransition' s botCtx
    =   SomeTransition <$> runBotContextParser (parseTransition @t @s @s' @m s) botCtx
    <|> parseSomeTransition' @s @ts s botCtx

type IsState :: Type -> (Type -> Type) -> Constraint
class IsState a m where
  type StateMessage a :: MessageKind

  extractMessageContext :: Applicative m => a -> m (MessageContext a)
  default extractMessageContext ::
    ( Applicative m
    , IsMessage (Proper' (StateMessage a)) ctx
    , HasTaggedContext ctx a
    ) => a -> m (MessageContext a)
  extractMessageContext _ = pure $ MessageContext EmptyTaggedContext

type MessageContext :: k -> Type
data MessageContext a where
  MessageContext ::
    ( IsMessage (Proper' (StateMessage a)) ctx
    , HasTaggedContext ctx0 a
    , ctx ~ ctx1 ++ ctx0
    ) => TaggedContext ctx1 -> MessageContext a

type SomeState :: [(Type, [Type])] -> (Type -> Type) -> Type
data SomeState fsa m where
  SomeState :: (Extract' s fsa ts, HasState s fsa ts m) => s -> SomeState fsa m

class IsTransition t s s' m | t s -> s' where
  parseTransition :: s -> BotContextParser t
  handleTransition :: t -> s -> m s'

type SomeTransitionFrom :: Type -> [(Type, [Type])] -> (Type -> Type) -> Type
data SomeTransitionFrom s fsa m where
  SomeTransition :: ( IsTransition t s s' m, Applicative m, Extract' s' fsa ts
                    , HasState s' fsa ts m, IsState s' m, IsState s m)
                 => t -> SomeTransitionFrom s fsa m

