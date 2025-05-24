{-# LANGUAGE DerivingVia #-}

module Telegram.Bot.FSAfe.FSA.ParseTransition
  ( ParseTransition(..), ParseTransitionFrom(..)
  , Command(..), Command'(..), CommandRead(..), CommandUnit(..)
  , CallbackQueryData(..)
  , Or(..)
  ) where

import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT(..))
import Data.Coerce (Coercible, coerce)
import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.String (IsString (..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

import qualified Data.Text as T

import Telegram.Bot.DSL.Classes.IsUnit (IsUnit (..))

import Telegram.Bot.FSAfe.BotContextParser (BotContextParser, command, callbackQueryDataRead)
import Telegram.Bot.FSAfe.Message.ReplyMarkup.IsCallbackQuery (IsCallbackQuery(..))

class ParseTransition t where
  parseTransition :: BotContextParser t

instance {-# OVERLAPPABLE #-}
         ParseTransition t => ParseTransitionFrom s t where
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
instance IsCallbackQuery a => ParseTransition (CallbackQueryData a) where
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

