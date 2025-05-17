{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Main where

import Telegram.Bot.FSAfe.Start (getEnvToken, hoistStartKeyedBot_)
import Telegram.Bot.FSAfe.BotContextParser (callbackQueryDataRead, command)
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

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data PizzaOrder
  = PizzaOrder
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving stock (Read, Show, Generic)
    deriving IsCallbackData via ReadShow PizzaOrder

data PizzaContext
  = PizzaContext
  { availableSizes :: [PizzaSize]
  , availableToppings :: [Topping]
  }

data PizzaSize = ExtraSmall | Small | Medium | Large | ExtraLarge
  deriving stock (Show, Read, Eq, Enum, Bounded)
  deriving IsCallbackData via ReadShow PizzaSize

instance HasTaggedContext '[ '("self", PizzaSize)] PizzaSize where
  getTaggedContext = andLet @"self"

data Topping = Cheese | Pepperoni | Mushrooms | Olives | Pineapples
  deriving stock (Show, Read, Eq, Enum, Bounded)

allValues :: (Enum a, Bounded a) => [a]
allValues = [minBound..maxBound]

data InitialState = InitialState deriving stock Generic
instance Monad m => IsState InitialState m where
  type StateMessage InitialState = AsMessage "Try /start"

newtype ConfirmingOrder = ConfirmingOrder
  { pizza :: PizzaOrder }
  deriving stock Generic
instance Monad m => IsState ConfirmingOrder m where
  type StateMessage ConfirmingOrder
    =  VarShow "pizza"
    :\ UnitCallbackBtn "Confirm" Confirm

data SelectingSize0 = SelectingSize0
  deriving stock Generic
instance MonadReader PizzaContext m => IsState SelectingSize0 m where
  type StateMessage SelectingSize0
    =  AsMessage "Please, select size of your pizza:"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"

  extractMessageContext _ = do
    availableSizes <- asks availableSizes
    return $ MessageContext
      $  Tagged @"pizzaSizes" (SelectSize <$> availableSizes)
      :. EmptyTaggedContext

newtype SelectingSize = SelectingSize
  { selectedSize :: PizzaSize }
  deriving stock Generic
instance MonadReader PizzaContext m => IsState SelectingSize m where
  type StateMessage SelectingSize
    =  F"You selected {show selectedSize} size"
    :\  "You selected " :|: VarShow "selectedSize" :|: " size"
    :\ CallbackButtons (VarShow "size") SelectSize "pizzaSizes"
    :\ UnitCallbackBtn "Confirm" Confirm

  extractMessageContext (SelectingSize selectedSize) = do
    availableSizes <- asks availableSizes
    return $ MessageContext
      $ andLet @"pizzaSizes" (SelectSize <$> filter (/= selectedSize) availableSizes)

data SelectingToppings = SelectingToppings
  { size :: PizzaSize
  , toppings :: [Topping]
  } deriving stock Generic
instance MonadReader PizzaContext m => IsState SelectingToppings m where
  type StateMessage SelectingToppings
    =  "Please, select toppings for your pizza:"
    :\ Buttons "toppingButtons"
    :\ UnitCallbackBtn "Confirm" Confirm

  extractMessageContext SelectingToppings{toppings} = do
    availableToppings <- asks availableToppings
    return $ MessageContext
      $ andLet @"toppingButtons" (f <$> availableToppings)
    where f s = if s `elem` toppings
            then callbackButton ("✓" <> tshow s) (RemoveTopping s)
            else callbackButton (tshow s) (AddTopping s)

data StartSelectingSize = StartSelectingSize
startSelectingSize :: Applicative m => Transition StartSelectingSize InitialState SelectingSize0 m
startSelectingSize = Transition{..} where
  parseTransition _ = StartSelectingSize <$ command "start"
  handleTransition _ _ = pure SelectingSize0

newtype SelectSize = SelectSize { size :: PizzaSize }
  deriving stock (Show, Read, Generic)
  deriving IsCallbackData via ReadShow SelectSize
selectSize0 :: Applicative m => Transition SelectSize SelectingSize0 SelectingSize m
selectSize0 = Transition{..} where
  parseTransition _ = callbackQueryDataRead @SelectSize
  handleTransition (SelectSize size) _ = pure $ SelectingSize size

selectSize :: Applicative m => Transition SelectSize SelectingSize SelectingSize m
selectSize = Transition{..} where
  parseTransition _ = callbackQueryDataRead @SelectSize
  handleTransition (SelectSize size) _ = pure $ SelectingSize size

newtype AddTopping = AddTopping Topping
  deriving stock (Show, Read)
  deriving IsCallbackData via ReadShow AddTopping
addTopping :: Applicative m => Transition AddTopping SelectingToppings SelectingToppings m
addTopping = Transition{..} where
  parseTransition _ = callbackQueryDataRead @AddTopping
  handleTransition (AddTopping topping) SelectingToppings{toppings, ..} =
    pure $ SelectingToppings{toppings = topping:toppings, ..}

newtype RemoveTopping = RemoveTopping Topping
  deriving stock (Show, Read)
  deriving IsCallbackData via ReadShow RemoveTopping
removeTopping :: Applicative m => Transition RemoveTopping SelectingToppings SelectingToppings m
removeTopping = Transition{..} where
  parseTransition _ = callbackQueryDataRead @RemoveTopping
  handleTransition (RemoveTopping topping) SelectingToppings{toppings, ..} =
    pure $ SelectingToppings{toppings = filter (/= topping) toppings, ..}

data Confirm = Confirm
  deriving stock (Show, Read, Generic)
  deriving anyclass IsUnit
  deriving IsCallbackData via ReadShow Confirm
confirmSize :: Applicative m => Transition Confirm SelectingSize SelectingToppings m
confirmSize = Transition{..} where
  parseTransition _
    =   callbackQueryDataRead @Confirm
    <|> Confirm <$ command "confirm"
  handleTransition Confirm (SelectingSize size) =
    pure $ SelectingToppings size []

confirmToppings :: Applicative m => Transition Confirm SelectingToppings ConfirmingOrder m
confirmToppings = Transition{..} where
  parseTransition _
    =   callbackQueryDataRead @Confirm
    <|> Confirm <$ command "confirm"
  handleTransition Confirm SelectingToppings{..} =
    pure $ ConfirmingOrder PizzaOrder{..}

confirmOrder :: Applicative m => Transition Confirm ConfirmingOrder InitialState m
confirmOrder = Transition{..} where
  parseTransition _
    =   callbackQueryDataRead @Confirm
    <|> Confirm <$ command "confirm"
  handleTransition Confirm ConfirmingOrder{} =
    pure InitialState

newtype AppM a = AppM { runAppM :: Reader PizzaContext a }
  deriving newtype (Functor, Applicative, Monad, MonadReader PizzaContext)

type a :>- b = Transition a b
type ab :-> c = ab c
type abc :@ m = abc m
type FSA =
 '[ '(InitialState,
      '[Transition StartSelectingSize InitialState SelectingSize0 AppM])
  , '(SelectingSize0,
      '[Transition SelectSize SelectingSize0 SelectingSize AppM])
  , '(SelectingSize,
      '[ Transition SelectSize SelectingSize SelectingSize AppM
       , Transition Confirm SelectingSize SelectingToppings AppM
       ]
     )
  , '(SelectingToppings,
      '[ AddTopping    :>- SelectingToppings :-> SelectingToppings :@ AppM
       , RemoveTopping :>- SelectingToppings :-> SelectingToppings :@ AppM
       , Confirm       :>- SelectingToppings :-> ConfirmingOrder   :@ AppM
       ]
     )
  , '(ConfirmingOrder,
      '[Transition Confirm ConfirmingOrder InitialState AppM])
  ]
fsa :: HList' FSA AppM
fsa = HCons' @InitialState      (HCons startSelectingSize HNil)
    $ HCons' @SelectingSize0    (HCons selectSize0 HNil)
    $ HCons' @SelectingSize     (HCons selectSize (HCons confirmSize HNil))
    $ HCons' @SelectingToppings (HCons addTopping (HCons removeTopping (HCons confirmToppings HNil)))
    $ HCons' @ConfirmingOrder   (HCons confirmOrder HNil)
      HNil'

main :: IO ()
main = do
  tgToken <- getEnvToken "TELEGRAM_BOT_TOKEN"
  putStrLn "bot is running"
  hoistStartKeyedBot_ fsa nt updateChatId InitialState tgToken
  where
    nt = return . flip runReader ctx . runAppM
    ctx = PizzaContext
      { availableSizes    = allValues `except` Small
      , availableToppings = allValues `except` Pineapples
      }

    values `except` value = filter (/= value) values
