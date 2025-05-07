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

import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartKeyedBot_)
import Telegram.Bot.FSAfe.BotContextParser (callbackQueryDataRead, command, runBotContextParser)
import Telegram.Bot.FSAfe.FSA (MessageContext(..), IsState(..), IsTransition(..), SomeTransitionFrom(..))

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

data State
  = InitialState
  | SelectingSize0
  | SelectingSize
  | SelectingToppings
  | ConfirmingOrder

instance IsState 'InitialState AppM where
  data StateData 'InitialState = InitialStateD deriving Generic
  type StateMessage 'InitialState = AsMessage "Try /start"
  parseTransition InitialStateD = runBotContextParser
    $ SomeTransition StartSelectingSize <$ command "start"

instance IsState 'SelectingSize0 AppM where
  data StateData 'SelectingSize0 = SelectingSize0D deriving Generic

  type StateMessage 'SelectingSize0
    =  "Please, select size of your pizza:"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"

  parseTransition _ = runBotContextParser
    $ SomeTransition <$> callbackQueryDataRead @SelectSize

  extractMessageContext _ = do
    availableSizes <- asks availableSizes
    return $ MessageContext
      $  Tagged @"pizzaSizes" (SelectSize <$> availableSizes)
      :. EmptyTaggedContext

instance IsState 'SelectingSize AppM where
  newtype StateData 'SelectingSize = SelectingSizeD
    { selectedSize :: PizzaSize }
    deriving Generic

  type StateMessage 'SelectingSize
    =  F"You selected {show selectedSize} size"
    :\  "You selected " :|: VarShow "selectedSize" :|: " size"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"
    :\ UnitCallbackBtn "Confirm" Confirm

  parseTransition SelectingSizeD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @SelectSize
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

  extractMessageContext (SelectingSizeD selectedSize) = do
    availableSizes <- asks availableSizes
    return $ MessageContext
      $ andLet @"pizzaSizes" (SelectSize <$> filter (/= selectedSize) availableSizes)

instance IsState 'SelectingToppings AppM where
  data StateData 'SelectingToppings
    = SelectingToppingsD
    { size :: PizzaSize
    , toppings :: [Topping]
    } deriving Generic

  type StateMessage 'SelectingToppings
    =  "Please, select toppings for your pizza:"
    :\ Buttons "toppingButtons"
    :\ UnitCallbackBtn "Confirm" Confirm

  extractMessageContext SelectingToppingsD{toppings} = do
    availableToppings <- asks availableToppings
    return $ MessageContext
      $ andLet @"toppingButtons" (f <$> availableToppings)
    where f s = if s `elem` toppings
            then callbackButton ("✓" <> tshow s) (RemoveTopping s)
            else callbackButton (tshow s) (AddTopping s)
  parseTransition SelectingToppingsD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @AddTopping
    <|> SomeTransition <$> callbackQueryDataRead @RemoveTopping
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

instance IsState 'ConfirmingOrder AppM where
  newtype StateData 'ConfirmingOrder = ConfirmingOrderD
    { pizza :: PizzaOrder }
    deriving Generic

  type StateMessage 'ConfirmingOrder
    =  VarShow "pizza"
    :\ UnitCallbackBtn "Confirm" Confirm

  parseTransition ConfirmingOrderD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

data Confirm = Confirm
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow Confirm

instance IsUnit Confirm where
  unitValue = Confirm

data StartSelectingSize = StartSelectingSize
instance IsTransition StartSelectingSize 'InitialState AppM 'SelectingSize0 where
  handleTransition StartSelectingSize InitialStateD =
    pure SelectingSize0D

newtype SelectSize = SelectSize { size :: PizzaSize }
  deriving (Show, Read, Generic)
  deriving IsCallbackData via ReadShow SelectSize

instance IsTransition SelectSize 'SelectingSize0 AppM 'SelectingSize where
  handleTransition (SelectSize size) SelectingSize0D{} =
    pure $ SelectingSizeD size

instance IsTransition SelectSize 'SelectingSize AppM 'SelectingSize where
  handleTransition (SelectSize size) SelectingSizeD{} =
    pure $ SelectingSizeD size

instance IsTransition Confirm 'SelectingSize AppM 'SelectingToppings where
  handleTransition Confirm (SelectingSizeD size) =
    pure $ SelectingToppingsD size []

newtype AddTopping = AddTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow AddTopping
instance IsTransition AddTopping 'SelectingToppings AppM 'SelectingToppings where
  handleTransition (AddTopping topping) SelectingToppingsD{toppings, ..} =
    pure $ SelectingToppingsD{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow RemoveTopping
instance IsTransition RemoveTopping 'SelectingToppings AppM 'SelectingToppings where
  handleTransition (RemoveTopping topping) SelectingToppingsD{toppings, ..} =
    pure $ SelectingToppingsD{toppings = filter (/= topping) toppings, ..}

instance IsTransition Confirm 'SelectingToppings AppM 'ConfirmingOrder where
  handleTransition Confirm SelectingToppingsD{..} = do
    pure $ ConfirmingOrderD PizzaOrder{..}

instance IsTransition Confirm 'ConfirmingOrder AppM 'InitialState where
  handleTransition Confirm ConfirmingOrderD{} = do
    pure InitialStateD

newtype AppM a = AppM { runAppM :: Reader PizzaContext a }
  deriving (Functor, Applicative, Monad, MonadReader PizzaContext)

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  hoistStartKeyedBot_ nt updateChatId InitialStateD tgToken
  where
    nt = pure . flip runReader ctx . runAppM
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
