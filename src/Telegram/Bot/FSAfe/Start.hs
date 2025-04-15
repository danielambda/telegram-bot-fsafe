{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}

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

import Telegram.Bot.FSAfe.BotM (BotContext(..), runBotM, BotM, MonadBot)
import Telegram.Bot.FSAfe.FSA (SomeStateData(..), IsState(..), FSAfeM)
import Control.Monad (void)
import Data.Function ((&))
import Data.String (fromString)
import System.Environment (getEnv)
import Telegram.Bot.FSAfe.Start.Internal (tryAdvanceState, startBotGeneric)

getEnvToken :: String -> IO Tg.Token
getEnvToken = fmap fromString . getEnv

startBot_ :: (FSAfeM ~ BotM, IsState a) => StateData a -> Tg.Token -> IO ()
startBot_ = hoistStartBot_ id

startBot :: (FSAfeM ~ BotM, IsState a) => StateData a -> Tg.Token -> IO (Either ClientError ())
startBot = hoistStartBot id

hoistStartBot_
  :: forall k { a :: k }. (MonadBot FSAfeM, IsState a)
  => (forall x. FSAfeM x -> BotM x) -> StateData a -> Tg.Token -> IO ()
hoistStartBot_ nt state token = void $ hoistStartBot nt state token

hoistStartBot
  :: forall k { a :: k }. (MonadBot FSAfeM, IsState a)
  => (forall x. FSAfeM x -> BotM x) -> StateData a -> Tg.Token -> IO (Either ClientError ())
hoistStartBot nt = startBotGeneric updateState . SomeStateData
  where
  updateState botUser state update =
    runBotM (nt $ tryAdvanceState state) (BotContext botUser update)

startKeyedBot_
  :: forall k { a :: k } key. (FSAfeM ~ BotM, Hashable key, IsState a)
  => (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO ()
startKeyedBot_ = hoistStartKeyedBot_ id

startKeyedBot
  :: forall k { a :: k } key. (FSAfeM ~ BotM, Hashable key, IsState a)
  => (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO (Either ClientError ())
startKeyedBot = hoistStartKeyedBot id

hoistStartKeyedBot_
  :: forall k { a :: k } key. (MonadBot FSAfeM, Hashable key, IsState a)
  => (forall x. FSAfeM x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> StateData a
  -> Tg.Token
  -> IO ()
hoistStartKeyedBot_ nt toKey initialState token = void $
  hoistStartKeyedBot nt toKey initialState token

hoistStartKeyedBot
  :: forall k { a :: k } key. (MonadBot FSAfeM, Hashable key, IsState a)
  => (forall x. FSAfeM x -> BotM x)
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
    nextState <- runBotM (nt $ tryAdvanceState state) (BotContext botUser update)
    return $ stateMap & HM.insert key nextState

