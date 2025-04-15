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
import Telegram.Bot.FSAfe (andLet, let', TextEntity(..), (:|:), CallbackBtn, AsMessage, (:\), ButtonEntity(..), TaggedContext(..), Tagged(..), Aboba(..))

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data PizzaOrder
  = PizzaOrder
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving (Show)

data PizzaContext
  = PizzaContext
  { availableSizes :: [PizzaSize]
  , availableToppings :: [Topping]
  }

data PizzaSize = ExtraSmall | Small | Medium | Large | ExtraLarge
  deriving (Show, Read, Eq, Enum, Bounded)

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
    :\ CallbackButtons PizzaSize "selectSize"

  parseTransition SelectingSize0D = runBotContextParser
    $ SomeTransition <$> callbackQueryDataRead @SelectSize

  toMessageData SelectingSize0D = do
    availableSizes <- asks availableSizes
    return $ Aboba $ Tagged @"selectSize" (f, availableSizes) :. EmptyTaggedContext
    where f s = asCallbackButton (tshow s) (SelectSize s)

instance IsState 'SelectingSize where
  newtype StateData 'SelectingSize = SelectingSizeD PizzaSize

  type StateMessage 'SelectingSize
    =  "You selected " :|: Var "selectedSize" :|: " size"
    :\ CallbackButtons PizzaSize "selectSize"
    :\ CallbackBtn "Confirm" "Confirm"
    -- TODO make this into actual type-safe transition callback

  parseTransition SelectingSizeD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @SelectSize
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

  toMessageData (SelectingSizeD selectedSize) = do
    availableSizes <- asks availableSizes
    return $ Aboba
      $ let'   @"selectedSize" (tshow selectedSize)
      $ andLet @"selectSize" (f, filter (/= selectedSize) availableSizes)
    where f s = asCallbackButton (tshow s) (SelectSize s)

instance IsState 'SelectingToppings where
  data StateData 'SelectingToppings
    = SelectingToppingsD
    { size :: PizzaSize
    , toppings :: [Topping]
    }

  type StateMessage 'SelectingToppings
    =  "Please, select toppings for your pizza:"
    :\ CallbackButtons Topping "selectTopping"
    :\ CallbackBtn "Confirm" "Confirm"

  toMessageData SelectingToppingsD{toppings} = do
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
    :\ CallbackBtn "Confirm" "Confirm"

  parseTransition ConfirmingOrderD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

  toMessageData (ConfirmingOrderD pizza) = do
    return $ Aboba $ andLet @"pizza" (tshow pizza)

data Confirm = Confirm
  deriving (Show, Read)

data StartSelectingSize = StartSelectingSize
instance IsTransition StartSelectingSize 'InitialState 'SelectingSize0 where
  handleTransition StartSelectingSize InitialStateD =
    pure SelectingSize0D

newtype SelectSize = SelectSize PizzaSize deriving (Show, Read)
instance IsTransition SelectSize 'SelectingSize0 'SelectingSize where
  handleTransition (SelectSize size) SelectingSize0D{} =
    pure $ SelectingSizeD size

instance IsTransition SelectSize 'SelectingSize 'SelectingSize where
  handleTransition (SelectSize size) SelectingSizeD{} =
    pure $ SelectingSizeD size

instance IsTransition Confirm 'SelectingSize 'SelectingToppings where
  handleTransition Confirm (SelectingSizeD size) =
    pure $ SelectingToppingsD size []

newtype AddTopping = AddTopping Topping deriving (Show, Read)
instance IsTransition AddTopping 'SelectingToppings 'SelectingToppings where
  handleTransition (AddTopping topping) SelectingToppingsD{toppings, ..} =
    pure $ SelectingToppingsD{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping deriving (Show, Read)
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
