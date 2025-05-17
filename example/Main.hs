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
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartKeyedBot_)
import Telegram.Bot.FSAfe.BotContextParser (callbackQueryDataRead, command, runBotContextParser)
import Telegram.Bot.FSAfe.FSA (MessageContext(..), IsState(..), HList'(..), HList(..), Transition(..))

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
import Data.Proxy (Proxy(..))

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

data InitialState = InitialState deriving Generic

data SelectingSize0 = SelectingSize0 deriving Generic

newtype SelectingSize = SelectingSize
  { selectedSize :: PizzaSize }
  deriving Generic

data SelectingToppings = SelectingToppings
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving Generic

newtype ConfirmingOrder = ConfirmingOrder
  { pizza :: PizzaOrder }
  deriving Generic

-- instance IsState InitialState AppM where
--   type StateMessage InitialState = AsMessage "Try /start"
--
-- instance IsState SelectingSize0 AppM where
--   type StateMessage SelectingSize0
--     =  "Please, select size of your pizza:"
--     :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"
--
--   extractMessageContext _ = do
--     availableSizes <- asks availableSizes
--     return $ MessageContext
--       $  Tagged @"pizzaSizes" (SelectSize <$> availableSizes)
--       :. EmptyTaggedContext
--
-- instance IsState SelectingSize AppM where
--   type StateMessage SelectingSize
--     =  F"You selected {show selectedSize} size"
--     :\  "You selected " :|: VarShow "selectedSize" :|: " size"
--     :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"
--     :\ UnitCallbackBtn "Confirm" Confirm
--
--   extractMessageContext (SelectingSize selectedSize) = do
--     availableSizes <- asks availableSizes
--     return $ MessageContext
--       $ andLet @"pizzaSizes" (SelectSize <$> filter (/= selectedSize) availableSizes)
--
-- instance IsState SelectingToppings AppM where
--   type StateMessage SelectingToppings
--     =  "Please, select toppings for your pizza:"
--     :\ Buttons "toppingButtons"
--     :\ UnitCallbackBtn "Confirm" Confirm
--
--   extractMessageContext SelectingToppings{toppings} = do
--     availableToppings <- asks availableToppings
--     return $ MessageContext
--       $ andLet @"toppingButtons" (f <$> availableToppings)
--     where f s = if s `elem` toppings
--             then callbackButton ("✓" <> tshow s) (RemoveTopping s)
--             else callbackButton (tshow s) (AddTopping s)
--
-- instance IsState ConfirmingOrder AppM where
--   type StateMessage ConfirmingOrder
--     =  VarShow "pizza"
--     :\ UnitCallbackBtn "Confirm" Confirm

data StartSelectingSize = StartSelectingSize
-- startSelectingSize = Transition
--   { parseTransition = \_ -> StartSelectingSize <$ command "start"
--   , handleTransition = \StartSelectingSize InitialState -> pure SelectingSize0
--   }

newtype SelectSize = SelectSize { size :: PizzaSize }
  deriving (Show, Read, Generic)
  deriving IsCallbackData via ReadShow SelectSize
--
-- instance IsTransition SelectSize SelectingSize0 AppM SelectingSize where
--   parseTransition _ = callbackQueryDataRead @SelectSize
--
--   handleTransition (SelectSize size) SelectingSize0{} =
--     pure $ SelectingSize size
--
-- instance IsTransition SelectSize SelectingSize AppM SelectingSize where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @SelectSize
--
--   handleTransition (SelectSize size) SelectingSize{} =
--     pure $ SelectingSize size

newtype AddTopping = AddTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow AddTopping

-- instance IsTransition AddTopping SelectingToppings AppM SelectingToppings where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @AddTopping
--
--   handleTransition (AddTopping topping) SelectingToppings{toppings, ..} =
--     pure $ SelectingToppings{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping
  deriving (Show, Read)
  deriving IsCallbackData via ReadShow RemoveTopping

-- instance IsTransition RemoveTopping SelectingToppings AppM SelectingToppings where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @RemoveTopping
--
--   handleTransition (RemoveTopping topping) SelectingToppings{toppings, ..} =
--     pure $ SelectingToppings{toppings = filter (/= topping) toppings, ..}

data Confirm = Confirm
  deriving (Show, Read, Generic, IsUnit)
  deriving IsCallbackData via ReadShow Confirm

-- instance IsTransition Confirm SelectingSize AppM SelectingToppings where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @Confirm
--     <|> Confirm <$ command "confirm"
--
--   handleTransition Confirm (SelectingSize size) =
--     pure $ SelectingToppings size []
--
-- instance IsTransition Confirm SelectingToppings AppM ConfirmingOrder where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @Confirm
--     <|> Confirm <$ command "confirm"
--
--   handleTransition Confirm SelectingToppings{..} = do
--     pure $ ConfirmingOrder PizzaOrder{..}
--
-- instance IsTransition Confirm ConfirmingOrder AppM InitialState where
--   parseTransition _ = runBotContextParser
--     $ callbackQueryDataRead @Confirm
--     <|> Confirm <$ command "confirm"
--
--   handleTransition Confirm ConfirmingOrder{} = do
--     pure InitialState

newtype AppM a = AppM { runAppM :: Reader PizzaContext a }
  deriving (Functor, Applicative, Monad, MonadReader PizzaContext)

data A = A deriving Generic
data B = B deriving Generic
instance IsState A where
  type StateMessage A = AsMessage "a"
instance IsState B where
  type StateMessage B = AsMessage "b"
data T0 = T0
data T1 = T1
fsa = HCons' (Proxy @B) (s $ Transition (\B _ -> Just T0) (\T0 B -> A)) (
      HCons' (Proxy @A) (s $ Transition (\A _ -> Just T1) (\T1 A -> B))
      HNil'
      )

s :: a -> HList '[a]
s a = HCons a HNil

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  hoistStartKeyedBot_ fsa nt updateChatId B tgToken
  where
    -- nt = pure . flip runReader ctx . runAppM
    nt = id
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
