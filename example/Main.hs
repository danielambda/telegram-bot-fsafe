{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartBot)
import Telegram.Bot.FSAfe.BotContextParser (callbackQueryDataRead, command, runBotContextParser)
import Telegram.Bot.FSAfe.FSA
import Data.Proxy

import Telegram.Bot.API as Tg (updateChatId)
import Control.Monad.Reader (Reader, runReader, MonadReader (..), asks)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Telegram.Bot.DSL
  ( CallbackButtons, HasTaggedContext(..), UnitCallbackBtn, IsUnit(..), ReadShow(..)
  , IsCallbackData, andLet, VarShow, F
  , (:|:), (:\), AsMessage
  , TaggedContext(..), Tagged(..)
  , callbackButton, Buttons
  )
import GHC.Generics (Generic)

type FSA =
 '[ A :>- T0
  , B :>- T1
  ]

data A = A deriving Generic
data B = B deriving Generic
data T0 = T0
data T1 = T1

instance IsState A AppM where
  type StateMessage A = "I am in state" :\ "AAAAAAAA"

instance IsState B AppM where
  type StateMessage B = "I am in state" :\ "BBBBBBBB"

instance IsTransition T0 A B AppM where
  parseTransition _ _ = Just T0
  handleTransition _ _ = pure B

instance IsTransition T1 B A AppM where
  parseTransition _ _ = Just T1
  handleTransition _ _ = pure A

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data PizzaOrder
  = PizzaOrder
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving (Read, Show, Generic)
    deriving IsCallbackData via ReadShow PizzaOrder

data PizzaContext
  = PizzaContext
  { availableSizes :: [PizzaSize]
  , availableToppings :: [Topping]
  }

data PizzaSize = ExtraSmall | Small | Medium | Large | ExtraLarge
  deriving (Show, Read, Eq, Enum, Bounded)
  deriving IsCallbackData via ReadShow PizzaSize

instance HasTaggedContext '[ '("self", PizzaSize)] PizzaSize where
  getTaggedContext = andLet @"self"

data Topping = Cheese | Pepperoni | Mushrooms | Olives | Pineapples
  deriving (Show, Read, Eq, Enum, Bounded)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..maxBound]

newtype AppM a = AppM { runAppM :: Reader PizzaContext a }
  deriving (Functor, Applicative, Monad, MonadReader PizzaContext)

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  _ <- hoistStartBot (Proxy @FSA) nt A tgToken
  return ()
  where
    nt = pure . flip runReader ctx . runAppM
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
