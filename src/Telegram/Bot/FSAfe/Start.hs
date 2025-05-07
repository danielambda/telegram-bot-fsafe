{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.FSAfe.Start
  ( getEnvToken
  , hoistStartBot, hoistStartBot_
  ,      startBot,      startBot_
  , hoistStartKeyedBot, hoistStartKeyedBot_
  ,      startKeyedBot,      startKeyedBot_
  , startBotGeneric
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Telegram.Bot.API as Tg
import Servant.Client (ClientError)

import Telegram.Bot.FSAfe.BotM (BotContext(..), runBotM, BotM)
import Telegram.Bot.FSAfe.FSA (SomeStateData(..), IsState(..))
import Control.Monad (void)
import Data.Function ((&))
import Data.String (fromString)
import System.Environment (getEnv)
import Telegram.Bot.FSAfe.Start.Internal (tryAdvanceState, startBotGeneric)

getEnvToken :: String -> IO Tg.Token
getEnvToken = fmap fromString . getEnv

startBot_ :: IsState a BotM => StateData a -> Tg.Token -> IO ()
startBot_ = hoistStartBot_ id

startBot :: IsState a BotM => StateData a -> Tg.Token -> IO (Either ClientError ())
startBot = hoistStartBot id

hoistStartBot_
  :: forall k { a :: k } m. IsState a m
  => (forall x. m x -> BotM x) -> StateData a -> Tg.Token -> IO ()
hoistStartBot_ nt state token = void $ hoistStartBot nt state token

hoistStartBot
  :: forall k { a :: k } m. IsState a m
  => (forall x. m x -> BotM x) -> StateData a -> Tg.Token -> IO (Either ClientError ())
hoistStartBot nt = startBotGeneric updateState . SomeStateData
  where
  updateState botUser state update =
    runBotM (tryAdvanceState nt state) (BotContext botUser update)

startKeyedBot_
  :: forall k { a :: k } key. (Hashable key, IsState a BotM)
  => (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO ()
startKeyedBot_ = hoistStartKeyedBot_ id

startKeyedBot
  :: forall k { a :: k } key. (Hashable key, IsState a BotM)
  => (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO (Either ClientError ())
startKeyedBot = hoistStartKeyedBot id

hoistStartKeyedBot_
  :: forall k { a :: k } key m. (Hashable key, IsState a m)
  => (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO ()
hoistStartKeyedBot_ nt toKey initialState token = void $
  hoistStartKeyedBot nt toKey initialState token

hoistStartKeyedBot
  :: forall k { a :: k } key m. (Hashable key, IsState a m)
  => (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO (Either ClientError ())
hoistStartKeyedBot nt toKey initialState = startBotGeneric updateStateMap HM.empty
  where
  updateStateMap botUser stateMap update = do
    let key = toKey update
    let defState = SomeStateData initialState
    let state = stateMap & HM.lookupDefault defState key
    runBotM (tryAdvanceState nt state) (BotContext botUser update) >>= \case
      Just nextState -> return $ Just $ stateMap & HM.insert key nextState
      Nothing -> return $ Just stateMap

