{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE UndecidableInstances #-}

module Main (main) where

import qualified Data.Text as T
import Telegram.Bot.API as Tg (updateChatId, InlineKeyboardButton)

import GHC.Generics (Generic)
import Data.Function ((&))
import Data.Proxy (Proxy(..))
import Control.Monad.Reader (ReaderT(..), Reader, runReader, MonadReader (..), asks)

-- import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartKeyedBot_)
-- import Telegram.Bot.FSAfe.Message (textMessage, ShowMode(..), withInlineKeyboard)
-- import Telegram.Bot.FSAfe.Message.ReplyMarkup (row, single)
-- import Telegram.Bot.FSAfe.Message.ReplyMarkup.IsCallbackQuery
--   (IsCallbackQuery(..), callbackButton)
-- import Telegram.Bot.FSAfe.FSA.StateMessage (StateMessage(..), StateMessageM(..))
-- import Telegram.Bot.FSAfe.FSA.ParseTransition
--   (ParseTransition, CallbackQueryData(..), CommandUnit(..), Or(..))
-- import Telegram.Bot.FSAfe.FSA.HandleTransition (HandleTransition(..))
-- import Telegram.Bot.FSAfe.Utils (ReadShow(..), IsUnit)
import Telegram.Bot.FSAfe
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
  deriving IsCallbackQuery via ReadShow PizzaSize

data Topping = Cheese | Pepperoni | Mushrooms | Olives | Pineapples
  deriving (Show, Read, Eq, Enum, Bounded)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..maxBound]

data InitialState = InitialState
instance StateMessage InitialState where
  stateMessage InitialState = Send $
    textMessage "You can make a new /order"

newtype ConfirmingOrder = ConfirmingOrder
  { pizza :: PizzaOrder }
instance StateMessage ConfirmingOrder where
  stateMessage ConfirmingOrder{..} = Edit $
    textMessage (tshow pizza)
    & withInlineKeyboard (single confirmButton)

data SelectingSize0 = SelectingSize0
instance MonadReader PizzaContext m => StateMessageM m SelectingSize0 where
  stateMessageM SelectingSize0 = Edit <$> do
    availableSizes <- asks availableSizes
    return $
      textMessage "Please, select size of your pizza:"
      & withInlineKeyboard
        (row $ map selectSizeButton availableSizes)
    where
      selectSizeButton size = callbackButton (tshow size) (SelectSize size)

newtype SelectingSize = SelectingSize
  { selectedSize :: PizzaSize }
instance MonadReader PizzaContext m => StateMessageM m SelectingSize where
  stateMessageM SelectingSize{..} = Edit <$> do
    availableSizes <- asks availableSizes
    return $
      textMessage ("You selected " <> tshow selectedSize <> " size")
      & withInlineKeyboard
        [ row $ map selectSizeButton $ filter (/= selectedSize) availableSizes
        , single confirmButton
        ]
    where
      selectSizeButton size = callbackButton (tshow size) (SelectSize size)

data SelectingToppings = SelectingToppings
  { size :: PizzaSize
  , toppings :: [Topping]
  }
instance MonadReader PizzaContext m => StateMessageM m SelectingToppings where
  stateMessageM SelectingToppings{toppings=selectedToppings} = Edit <$> do
    availableToppings <- asks availableToppings
    return $
      textMessage "Please, select toppings for your pizza:"
      & withInlineKeyboard
        [ row $ map selectToppingButton availableToppings
        , single confirmButton
        ]
    where
      selectToppingButton topping = if topping `elem` selectedToppings
        then callbackButton ("✓" <> tshow topping) (RemoveTopping topping)
        else callbackButton (       tshow topping) (AddTopping topping)

data Start = Start
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "start" Start
instance HandleTransition Start InitialState InitialState where
  handleTransition _ _ = InitialState

data BeginOrder = BeginOrder
  deriving (Generic, IsUnit)
  deriving ParseTransition via CommandUnit "order" BeginOrder
instance HandleTransition BeginOrder InitialState SelectingSize0 where
  handleTransition _ _ = SelectingSize0

newtype SelectSize = SelectSize { size :: PizzaSize }
  deriving (Show, Read)
  deriving IsCallbackQuery via ReadShow SelectSize
  deriving ParseTransition via CallbackQueryData SelectSize
instance HandleTransition SelectSize s SelectingSize where
  handleTransition (SelectSize size) _ = SelectingSize size

newtype AddTopping = AddTopping Topping
  deriving (Show, Read)
  deriving IsCallbackQuery via ReadShow AddTopping
  deriving ParseTransition via CallbackQueryData AddTopping
instance HandleTransition AddTopping SelectingToppings SelectingToppings where
  handleTransition (AddTopping topping) SelectingToppings{toppings, ..} =
    SelectingToppings{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping
  deriving (Show, Read)
  deriving IsCallbackQuery via ReadShow RemoveTopping
  deriving ParseTransition via CallbackQueryData RemoveTopping
instance HandleTransition RemoveTopping SelectingToppings SelectingToppings where
  handleTransition (RemoveTopping topping) SelectingToppings{toppings, ..} =
    SelectingToppings{toppings = filter (/= topping) toppings, ..}

data Confirm = Confirm
  deriving (Read, Show, Generic, IsUnit)
  deriving IsCallbackQuery via ReadShow Confirm
  deriving ParseTransition via (CommandUnit "confirm" `Or` CallbackQueryData) Confirm

confirmButton :: InlineKeyboardButton
confirmButton = callbackButton "Confirm" Confirm

instance HandleTransition Confirm SelectingSize SelectingToppings where
  handleTransition Confirm (SelectingSize size) = SelectingToppings size []

instance HandleTransition Confirm SelectingToppings ConfirmingOrder where
  handleTransition Confirm SelectingToppings{..} = ConfirmingOrder PizzaOrder{..}

instance HandleTransition Confirm ConfirmingOrder InitialState where
  handleTransition Confirm ConfirmingOrder{} = InitialState

newtype AppM a = AppM { runAppM :: Reader PizzaContext a }
  deriving newtype (Functor, Applicative, Monad, MonadReader PizzaContext)

type FSA =
 '[ '(InitialState, '[Start, BeginOrder])
  , '(SelectingSize0, '[SelectSize])
  , '(SelectingSize, '[SelectSize, Confirm])
  , '(SelectingToppings, '[AddTopping, RemoveTopping, Confirm])
  , '(ConfirmingOrder, '[Confirm])
  ]

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  hoistStartKeyedBot_ (Proxy @FSA) nt updateChatId InitialState tgToken
  where
    nt = return . flip runReader ctx . runAppM
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
