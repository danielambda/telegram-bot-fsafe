{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Telegram.Bot.FSAfe.FSA
  ( module Exports
  , SomeState(..)
  , SomeTransitionFrom(..)
  , HasState(..)
  ) where

import Telegram.Bot.FSAfe.FSA.HandleTransition as Exports
import Telegram.Bot.FSAfe.FSA.StateMessage as Exports
import Telegram.Bot.FSAfe.FSA.ParseTransition as Exports

import Data.Kind (Type, Constraint)

import Control.Applicative ((<|>))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Telegram.Bot.FSAfe.BotContextParser (runBotContextParser)
import GHC.TypeError (TypeError, ErrorMessage(..))
import Data.Type.Bool (If)

type HasState :: Type -> [(Type, [Type])] -> (Type -> Type) -> Constraint
class HasState s fsa m where
  parseSomeTransition :: s -> BotContext -> Maybe (SomeTransitionFrom s fsa m)

instance ( HasState' s ts fsa m
         , If (IsJust (Extract s fsa))
            ('Just ts ~ Extract s fsa)
            (TypeError
              (    ShowType fsa
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
         , StateMessageM m s
         , ParseTransitionFrom s t
         , HandleTransitionM t s s' m
         , StateMessageM m s'
         , HasState s' fsa m
         , HasState' s ts fsa m
         )
      => HasState' s (t ': ts) fsa m where
  parseSomeTransition' s botCtx
    =   SomeTransition <$> runBotContextParser (parseTransitionFrom @s @t s) botCtx
    <|> parseSomeTransition' @s @ts s botCtx

type SomeState :: [(Type, [Type])] -> (Type -> Type) -> Type
data SomeState fsa m where
  SomeState :: HasState s fsa m => s -> SomeState fsa m

type SomeTransitionFrom :: Type -> [(Type, [Type])] -> (Type -> Type) -> Type
data SomeTransitionFrom s fsa m where
  SomeTransition ::
    ( Applicative m
    , StateMessageM m s
    , HandleTransitionM t s s' m
    , StateMessageM m s'
    , HasState s' fsa m
    ) => t -> SomeTransitionFrom s fsa m


type Extract :: Type -> [(Type, k)] -> Maybe k
type family Extract s fsa where
  Extract _ '[] = 'Nothing
  Extract s ('(s, ts) ': fsa) = Just ts
  Extract s ('(x, xts) ': fsa) = Extract s fsa

type IsJust :: Maybe a -> Bool
type family IsJust m where
  IsJust ('Just _) = 'True
  IsJust 'Nothing = 'False
