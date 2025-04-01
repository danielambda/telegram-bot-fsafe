{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ExplicitForAll #-}

{-# OPTIONS_GHC -Wno-orphans #-} -- for FSAfeM type instance

module Main where

import Telegram.Bot.FSAfe
import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (ReaderT (..), MonadReader (..), asks)
import Control.Applicative ((<|>))
import Telegram.Bot.API as Tg
import qualified Data.Text as T

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

data PizzaSize = Small | Medium | Large
  deriving (Show, Read, Eq, Enum, Bounded)

data Topping = Cheese | Pepperoni | Mushrooms | Olives
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
  parseTransition InitialStateD = runBotContextParser
    $ SomeTransition StartSelectingSize <$ command "start"

instance IsState 'SelectingSize0 where
  data StateData 'SelectingSize0 = SelectingSize0D
  parseTransition SelectingSize0D = runBotContextParser
    $ SomeTransition <$> callbackQueryDataRead @SelectSize

instance IsState 'SelectingSize where
  data StateData 'SelectingSize = SelectingSizeD PizzaSize
  parseTransition SelectingSizeD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @SelectSize
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

instance IsState 'SelectingToppings where
  data StateData 'SelectingToppings
    = SelectingToppingsD
    { size :: PizzaSize
    , toppings :: [Topping]
    }
  parseTransition SelectingToppingsD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @AddTopping
    <|> SomeTransition <$> callbackQueryDataRead @RemoveTopping
    <|> SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

instance IsState 'ConfirmingOrder where
  newtype StateData 'ConfirmingOrder = ConfirmingOrderD PizzaOrder
  parseTransition ConfirmingOrderD{} = runBotContextParser
    $   SomeTransition <$> callbackQueryDataRead @Confirm
    <|> SomeTransition Confirm <$ command "confirm"

data Confirm = Confirm
  deriving (Show, Read)

data StartSelectingSize = StartSelectingSize
instance IsTransition StartSelectingSize 'InitialState 'SelectingSize0 where
  handleTransition StartSelectingSize InitialStateD{} = do
    availableSizes <- asks availableSizes
    reply $ (toReplyMessage "Please, select size of your pizza:")
      { replyMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          [(\s -> asCallbackButton (T.pack $ show s) (SelectSize s)) <$> availableSizes]
      }
    return SelectingSize0D

newtype SelectSize = SelectSize PizzaSize deriving (Show, Read)
instance IsTransition SelectSize 'SelectingSize0 'SelectingSize where
  handleTransition (SelectSize size) SelectingSize0D{} = do
    availableSizes <- asks availableSizes
    editUpdateMessage $ (toEditMessage $ "You selected " <> T.pack (show size) <> " size")
      { editMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          ((\s -> asCallbackButton (T.pack $ show s) (SelectSize s)) <$> (filter (/= size) availableSizes))
          :[[asCallbackButton "Confirm" Confirm]]
      }
    return $ SelectingSizeD size

instance IsTransition SelectSize 'SelectingSize 'SelectingSize where
  handleTransition (SelectSize size) SelectingSizeD{} = do
    availableSizes <- asks availableSizes
    editUpdateMessage $ (toEditMessage $ "You selected " <> T.pack (show size) <> " size")
      { editMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          ((\s -> asCallbackButton (T.pack $ show s) (SelectSize s)) <$> (filter (/= size) availableSizes))
          :[[asCallbackButton "Confirm" Confirm]]
      }
    return $ SelectingSizeD size

instance IsTransition Confirm 'SelectingSize 'SelectingToppings where
  handleTransition Confirm (SelectingSizeD size) = do
    availableToppings <- asks availableToppings
    editUpdateMessage $ (toEditMessage $ "Please, select toppings for your pizza:")
      { editMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          ((\s -> asCallbackButton (T.pack $ show s) (AddTopping s)) <$> availableToppings)
          :[[asCallbackButton "Confirm" Confirm]]
      }
    return $ SelectingToppingsD size []

newtype AddTopping = AddTopping Topping deriving (Show, Read)
instance IsTransition AddTopping 'SelectingToppings 'SelectingToppings where
  handleTransition (AddTopping topping) SelectingToppingsD{toppings, ..} = do
    let toppings' = topping:toppings
    availableToppings <- asks availableToppings
    editUpdateMessage $ (toEditMessage "Please, select toppings for your pizza:")
      { editMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          ((\s -> (if s `elem` toppings'
            then asCallbackButton ("✓" <> T.pack (show s)) (RemoveTopping s)
            else asCallbackButton (T.pack $ show s) (AddTopping s))
           ) <$> availableToppings
          ):[[asCallbackButton "Confirm" Confirm]]
      }
    return $ SelectingToppingsD{toppings = toppings', ..}

newtype RemoveTopping = RemoveTopping Topping deriving (Show, Read)
instance IsTransition RemoveTopping 'SelectingToppings 'SelectingToppings where
  handleTransition (RemoveTopping topping) SelectingToppingsD{toppings, ..} = do
    let toppings' = filter (/= topping) toppings
    availableToppings <- asks availableToppings
    editUpdateMessage $ (toEditMessage "Please, select toppings for your pizza:")
      { editMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
          ((\s -> (if s `elem` toppings'
            then asCallbackButton ("✓" <> T.pack (show s)) (RemoveTopping s)
            else asCallbackButton (T.pack $ show s) (AddTopping s))
           ) <$> availableToppings
          ):[[asCallbackButton "Confirm" Confirm]]
      }
    return $ SelectingToppingsD{toppings = toppings', ..}

instance IsTransition Confirm 'SelectingToppings 'ConfirmingOrder where
  handleTransition Confirm SelectingToppingsD{..} = do
    reply $ (toReplyMessage $ T.pack $ show PizzaOrder{..}){
      replyMessageReplyMarkup = Just $ Tg.SomeInlineKeyboardMarkup $ Tg.InlineKeyboardMarkup $
        [[asCallbackButton "Confirm" Confirm]]
    }
    return $ ConfirmingOrderD PizzaOrder{..}

instance IsTransition Confirm 'ConfirmingOrder 'InitialState where
  handleTransition Confirm (ConfirmingOrderD order) = do
    editUpdateMessage $ toEditMessage $ T.pack $ show order
    replyText "You can /start ordering another pizza!"
    return InitialStateD

data Cancel = Cancel

newtype AppM a = AppM { runAppM :: ReaderT PizzaContext BotM a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadReader PizzaContext)

instance MonadBot AppM where
  liftBot = AppM . lift

type instance FSAfeM = AppM

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn ("bot is running" :: String)
  hoistStartKeyedBot_ nt updateChatId InitialStateD tgToken
  where
    ctx = PizzaContext
      { availableSizes = allValues
      , availableToppings = allValues
      }
    nt = flip runReaderT ctx . runAppM

