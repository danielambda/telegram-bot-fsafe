{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE PatternSynonyms #-}

module Telegram.Bot.FSAfe.FSA
  ( IsState(..), IsStateM(..), SomeState(..)
  , SomeTransitionFrom(..)
  , HasState(..)
  , ParseTransition(..), ParseTransitionFrom(..)
  , HandleTransition(..), HandleTransitionM(..)
  , Command, pattern Command, Command'(..)
  , CommandUnit(..), CommandRead(..)
  , CallbackQueryData(..)
  , Or(..)
  ) where

import Data.Kind (Type, Constraint)

import Telegram.Bot.DSL (IsCallbackData, IsUnit (..))
import Telegram.Bot.FSAfe.Message (MessageShowMode)

import Control.Applicative ((<|>))

import Telegram.Bot.FSAfe.BotM (BotContext)
import Telegram.Bot.FSAfe.BotContextParser (BotContextParser, runBotContextParser, command, callbackQueryDataRead)
import GHC.TypeError (TypeError, ErrorMessage(..))
import Data.Type.Bool (If)
import GHC.Base (Symbol, Coercible)
import GHC.TypeLits (symbolVal, KnownSymbol)
import qualified Data.Text as T
import Data.Proxy (Proxy(..))
import Data.String (IsString (..))
import Data.Coerce (coerce)
import Control.Monad.Reader (ReaderT(..))

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
         , IsStateM m s
         , ParseTransitionFrom s t
         , HandleTransitionM t s s' m
         , IsStateM m s'
         , HasState s' fsa m
         , HasState' s ts fsa m
         )
      => HasState' s (t ': ts) fsa m where
  parseSomeTransition' s botCtx
    =   SomeTransition <$> runBotContextParser (parseTransitionFrom @s @t s) botCtx
    <|> parseSomeTransition' @s @ts s botCtx

class IsStateM m a where
  stateMessageM :: a -> m MessageShowMode

instance {-# OVERLAPPABLE #-}
         (Applicative m, IsState a) => IsStateM m a where
  stateMessageM = pure . stateMessage

class IsState a where
  stateMessage :: a -> MessageShowMode

type SomeState :: [(Type, [Type])] -> (Type -> Type) -> Type
data SomeState fsa m where
  SomeState :: HasState s fsa m => s -> SomeState fsa m

class ParseTransition t where
  parseTransition :: BotContextParser t

instance {-# OVERLAPPABLE #-} ParseTransition t => ParseTransitionFrom s t where
  parseTransitionFrom _ = parseTransition

class ParseTransitionFrom s t where
  parseTransitionFrom :: s -> BotContextParser t

type Command :: Symbol -> Type
newtype Command cmd = Command T.Text
  deriving ParseTransition via Command' cmd T.Text

type Command' :: Symbol -> Type -> Type
newtype Command' cmd t = Command' t
instance (IsString t, KnownSymbol cmd) => ParseTransition (Command' cmd t) where
  parseTransition
    = fmap (Command' . fromString . T.unpack)
    $ command
    $ T.pack
    $ symbolVal
    $ Proxy @cmd

newtype CommandRead cmd a = CommandRead a
instance (Read s, KnownSymbol cmd) => ParseTransition (CommandRead cmd s) where
  parseTransition
    = fmap (CommandRead . read . T.unpack)
    $ command
    $ T.pack
    $ symbolVal
    $ Proxy @cmd

newtype CommandUnit cmd a = CommandUnit a
instance (IsUnit s, KnownSymbol cmd) => ParseTransition (CommandUnit cmd s) where
  parseTransition
    = fmap (const $ CommandUnit unitValue)
    $ command
    $ T.pack
    $ symbolVal
    $ Proxy @cmd

newtype CallbackQueryData a = CallbackQueryData a
instance IsCallbackData a => ParseTransition (CallbackQueryData a) where
  parseTransition = CallbackQueryData <$> callbackQueryDataRead

type Or :: k -> l -> Type -> Type
newtype Or a b c = Or c
instance ( Coercible (a c) c, Coercible (b c) c
         , ParseTransition (a c)
         , ParseTransition (b c)
         )
      => ParseTransition (Or a b c) where
  parseTransition = fmap Or
    $   coerce (parseTransition @(a c))
    <|> coerce (parseTransition @(b c))

instance ( Coercible a c, Coercible (b c) c
         , ParseTransition a
         , ParseTransition (b c)
         ) => ParseTransition (Or a b c) where
  parseTransition = fmap Or
    $   coerce (parseTransition @a)
    <|> coerce (parseTransition @(b c))

instance ( Coercible (a c) c, Coercible b c
         , ParseTransition (a c)
         , ParseTransition b
         ) => ParseTransition (Or a b c) where
  parseTransition = fmap Or
    $   coerce (parseTransition @(a c))
    <|> coerce (parseTransition @b)

class HandleTransition t s s' | t s -> s' where
  handleTransition :: t -> s -> s'

instance {-# OVERLAPPABLE #-}
         (HandleTransition t s s', Applicative m) => HandleTransitionM t s s' m where
  handleTransitionM t s = pure $ handleTransition t s
  automaticallyHandleCallbackQueries = True

class HandleTransitionM t s s' m | t s -> s' where
  handleTransitionM :: t -> s -> m s'
  automaticallyHandleCallbackQueries :: Bool
  automaticallyHandleCallbackQueries = False

type SomeTransitionFrom :: Type -> [(Type, [Type])] -> (Type -> Type) -> Type
data SomeTransitionFrom s fsa m where
  SomeTransition ::
    ( Applicative m
    , IsStateM m s
    , HandleTransitionM t s s' m
    , IsStateM m s'
    , HasState s' fsa m
    ) => t -> SomeTransitionFrom s fsa m

