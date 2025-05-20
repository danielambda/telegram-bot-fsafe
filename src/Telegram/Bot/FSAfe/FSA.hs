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
  , IsTransition(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsMessage, MessageKind, Proper', HasTaggedContext (..), type (++))
import Telegram.Bot.DSL.TaggedContext  (TaggedContext (..))

import Control.Applicative ((<|>))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Telegram.Bot.FSAfe.BotContextParser (BotContextParser, runBotContextParser)
import GHC.TypeError (TypeError, ErrorMessage(..))
import Data.Type.Bool (If)

type Extract :: Type -> [(Type, k)] -> Maybe k
type family Extract s fsa where
  Extract _ '[] = 'Nothing
  Extract s ('(s, ts) ': fsa) = Just ts
  Extract s ('(x, xts) ': fsa) = Extract s fsa

type IsJust :: Maybe a -> Bool
type family IsJust m where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False

type HasState :: Type -> [(Type, [Type])] -> (Type -> Type) -> Constraint
class HasState s fsa m where
  parseSomeTransition :: s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance ( HasState' s ts fsa m
         , If (IsJust (Extract s fsa))
            ('Just ts ~ Extract s fsa)
            (TypeError
              ( ShowType fsa
              :<>: Text " does not specify transitions for state "
              :<>: ShowType s
              )
            )
         )
      => HasState s fsa m where
  parseSomeTransition = parseSomeTransition' @s @ts

type HasState' :: Type -> [Type] -> [(Type, [Type])] -> (Type -> Type) -> Constraint
class HasState' s ts fsa m where
  parseSomeTransition' :: s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance HasState' s '[] fsa m where
  parseSomeTransition' _ _ = Nothing

instance ( Applicative m
         , IsState s m
         , IsTransition t s s' m
         , IsState s' m
         , HasState s' fsa m
         , HasState' s ts fsa m
         )
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
  SomeState :: HasState s fsa m => s -> SomeState fsa m

class IsTransition t s s' m | t s -> s' where
  parseTransition :: s -> BotContextParser t
  handleTransition :: t -> s -> m s'

type SomeTransitionFrom :: Type -> [(Type, [Type])] -> (Type -> Type) -> Type
data SomeTransitionFrom s fsa m where
  SomeTransition ::
    ( Applicative m
    , IsState s m
    , IsTransition t s s' m
    , IsState s' m
    , HasState s' fsa m
    ) => t -> SomeTransitionFrom s fsa m

