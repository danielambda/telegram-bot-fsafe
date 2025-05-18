{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
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
import Telegram.Bot.FSAfe.FSA (SomeState(..), HasState)
import Control.Monad (void)
import Data.Function ((&))
import Data.String (fromString)
import System.Environment (getEnv)
import Telegram.Bot.FSAfe.Start.Internal (tryAdvanceState, startBotGeneric)
import Data.Proxy (Proxy)

getEnvToken :: String -> IO Tg.Token
getEnvToken = fmap fromString . getEnv

startBot_ :: (HasState a fsa ts BotM)
          => Proxy fsa -> a -> Tg.Token -> IO ()
startBot_ fsa = hoistStartBot_ fsa id

startBot :: (HasState a fsa ts BotM)
         => Proxy fsa -> a -> Tg.Token -> IO (Either ClientError ())
startBot fsa = hoistStartBot fsa id

hoistStartBot_
  :: forall fsa a m ts. (HasState a fsa ts m)
  => Proxy fsa -> (forall x. m x -> BotM x) -> a -> Tg.Token -> IO ()
hoistStartBot_ fsa nt state token = void $ hoistStartBot fsa nt state token

hoistStartBot
  :: forall fsa a m ts. (HasState a fsa ts m)
  => Proxy fsa -> (forall x. m x -> BotM x) -> a -> Tg.Token -> IO (Either ClientError ())
hoistStartBot _ nt = startBotGeneric updateState . SomeState @a @fsa @ts
  where
  updateState botUser state update =
    runBotM (tryAdvanceState nt state) (BotContext botUser update)

startKeyedBot_
  :: forall fsa a ts key. (Hashable key, HasState a fsa ts BotM)
  => Proxy fsa
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO ()
startKeyedBot_ fsa = hoistStartKeyedBot_ fsa id

startKeyedBot
  :: forall fsa a ts key. (Hashable key, HasState a fsa ts BotM)
  => Proxy fsa
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO (Either ClientError ())
startKeyedBot fsa = hoistStartKeyedBot fsa id

hoistStartKeyedBot_
  :: forall fsa a key m ts. (Hashable key, HasState a fsa ts m)
  => Proxy fsa
  -> (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO ()
hoistStartKeyedBot_ fsa nt toKey initialState token = void $
  hoistStartKeyedBot fsa nt toKey initialState token

hoistStartKeyedBot
  :: forall fsa a key m ts. (Hashable key, HasState a fsa ts m)
  => Proxy fsa
  -> (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO (Either ClientError ())
hoistStartKeyedBot _ nt toKey initialState = startBotGeneric updateStateMap HM.empty
  where
  updateStateMap botUser stateMap update = do
    let key = toKey update
    let defState = SomeState @a @fsa @ts initialState
    let state = stateMap & HM.lookupDefault defState key
    runBotM (tryAdvanceState nt state) (BotContext botUser update) >>= \case
      Just nextState -> return $ Just $ stateMap & HM.insert key nextState
      Nothing -> return $ Just stateMap

