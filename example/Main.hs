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
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for FSAfeM type instance

module Main where

import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartKeyedBot_)
import Telegram.Bot.FSAfe.BotContextParser (callbackQueryDataRead, command, runBotContextParser)
import Telegram.Bot.FSAfe.BotM (BotM, MonadBot(..))
import Telegram.Bot.FSAfe.FSA (IsState(..), IsTransition(..), SomeTransitionFrom(..), FSAfeM)
import Telegram.Bot.FSAfe.Reply (asCallbackButton)

import Telegram.Bot.API as Tg (updateChatId)
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..), asks)
import Control.Applicative ((<|>))
import qualified Data.Text as T
import Telegram.Bot.FSAfe (CallbackButtons, HasTaggedContext(..), Fields(..), UnitCallbackBtn, IsUnit(..), ReadShow(..), IsCallbackData, andLet, let', TextEntity(..), (:|:), CallbackBtn, AsMessage, (:\), ButtonEntity(..), TaggedContext(..), Tagged(..), Aboba(..))

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data PizzaOrder
  = PizzaOrder
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving (Read, Show)
    deriving IsCallbackData via ReadShow PizzaOrder
    deriving (HasTaggedContext '[ '("size", PizzaSize)]) via Fields '[ '("size", PizzaSize)] PizzaOrder

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

instance IsState 'InitialState where
  data StateData 'InitialState = InitialStateD
  type StateMessage 'InitialState = AsMessage "Try /start"
  parseTransition InitialStateD = runBotContextParser
    $ SomeTransition StartSelectingSize <$ command "start"

instance IsState 'SelectingSize0 where
  data StateData 'SelectingSize0 = SelectingSize0D

  type StateMessage 'SelectingSize0
    =  "Please, select size of your pizza:"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"

  parseTransition SelectingSize0D = runBotContextParser
    $ SomeTransition <$> callbackQueryDataRead @SelectSize

  extractMessageContext SelectingSize0D = do
    availableSizes <- asks availableSizes
    return $ Aboba $ Tagged @"pizzaSizes" (SelectSize <$> availableSizes) :. EmptyTaggedContext

instance IsState 'SelectingSize where
  newtype StateData 'SelectingSize = SelectingSizeD PizzaSize

  type StateMessage 'SelectingSize
    =  "You selected " :|: Var "selectedSize" :|: " size"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"
    :\ UnitCallbackBtn "Confirm" Confirm

  parseTransition SelectingSizeD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @SelectSize
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

  extractMessageContext (SelectingSizeD selectedSize) = do
    availableSizes <- asks availableSizes
    return $ Aboba
      $ let'   @"selectedSize" (tshow selectedSize)
      $ andLet @"pizzaSizes" (SelectSize <$> filter (/= selectedSize) availableSizes)

instance IsState 'SelectingToppings where
  data StateData 'SelectingToppings
    = SelectingToppingsD
    { size :: PizzaSize
    , toppings :: [Topping]
    }

  type StateMessage 'SelectingToppings
    =  "Please, select toppings for your pizza:"
    -- :\ CallbackButtons Topping "selectTopping" -- TODO  ?
    :\ UnitCallbackBtn "Confirm" Confirm

  extractMessageContext SelectingToppingsD{toppings} = do
    availableToppings <- asks availableToppings
    return $ Aboba
      $ andLet @"selectTopping" (f, availableToppings)
    where f s = if s `elem` toppings
            then asCallbackButton ("✓" <> tshow s) (RemoveTopping s)
            else asCallbackButton (tshow s) (AddTopping s)
  parseTransition SelectingToppingsD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @AddTopping
    <|> SomeTransition <$> callbackQueryDataRead @RemoveTopping
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

instance IsState 'ConfirmingOrder where
  newtype StateData 'ConfirmingOrder = ConfirmingOrderD PizzaOrder

  type StateMessage 'ConfirmingOrder
    =  AsMessage (Var "pizza")
    :\ CallbackBtn (VarShow "size") PizzaOrder "order"
    :\ UnitCallbackBtn "Confirm" Confirm

  parseTransition ConfirmingOrderD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

  extractMessageContext (ConfirmingOrderD pizza) = return $ Aboba
    $ let' @"order" PizzaOrder{size = Medium, toppings=[]}
    $ andLet @"pizza" (tshow pizza)

data Confirm = Confirm
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow Confirm

instance IsUnit Confirm where
  unitValue = Confirm

data StartSelectingSize = StartSelectingSize
instance IsTransition StartSelectingSize 'InitialState 'SelectingSize0 where
  handleTransition StartSelectingSize InitialStateD =
    pure SelectingSize0D

newtype SelectSize = SelectSize { size :: PizzaSize }
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow SelectSize
  deriving (HasTaggedContext '[ '("size", PizzaSize)]) via Fields '[ '("size", PizzaSize)] SelectSize

instance IsTransition SelectSize 'SelectingSize0 'SelectingSize where
  handleTransition (SelectSize size) SelectingSize0D{} =
    pure $ SelectingSizeD size

instance IsTransition SelectSize 'SelectingSize 'SelectingSize where
  handleTransition (SelectSize size) SelectingSizeD{} =
    pure $ SelectingSizeD size

instance IsTransition Confirm 'SelectingSize 'SelectingToppings where
  handleTransition Confirm (SelectingSizeD size) =
    pure $ SelectingToppingsD size []

newtype AddTopping = AddTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow AddTopping
instance IsTransition AddTopping 'SelectingToppings 'SelectingToppings where
  handleTransition (AddTopping topping) SelectingToppingsD{toppings, ..} =
    pure $ SelectingToppingsD{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow RemoveTopping
instance IsTransition RemoveTopping 'SelectingToppings 'SelectingToppings where
  handleTransition (RemoveTopping topping) SelectingToppingsD{toppings, ..} =
    pure $ SelectingToppingsD{toppings = filter (/= topping) toppings, ..}

instance IsTransition Confirm 'SelectingToppings 'ConfirmingOrder where
  handleTransition Confirm SelectingToppingsD{..} = do
    pure $ ConfirmingOrderD PizzaOrder{..}

instance IsTransition Confirm 'ConfirmingOrder 'InitialState where
  handleTransition Confirm ConfirmingOrderD{} = do
    pure InitialStateD

newtype AppM a = AppM { runAppM :: ReaderT PizzaContext BotM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PizzaContext)

instance MonadBot AppM where
  liftBot = AppM . lift

type instance FSAfeM = AppM

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  hoistStartKeyedBot_ nt updateChatId InitialStateD tgToken
  where
    nt = flip runReaderT ctx . runAppM
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
