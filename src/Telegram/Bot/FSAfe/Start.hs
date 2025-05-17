{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.FSAfe.Start
  ( getEnvToken
  , hoistStartBot, hoistStartBot_
  ,      startBot,      startBot_
  , hoistStartKeyedBot, hoistStartKeyedBot_
  -- ,      startKeyedBot,      startKeyedBot_
  , startBotGeneric
  ) where

import qualified Data.HashMap.Strict as HM
import Data.Hashable (Hashable)
import qualified Telegram.Bot.API as Tg
import Servant.Client (ClientError)

import Telegram.Bot.FSAfe.BotM (BotContext(..), runBotM, BotM)
import Telegram.Bot.FSAfe.FSA (SomeState(..), HList', HasState)
import Control.Monad (void)
import Data.Function ((&))
import Data.String (fromString)
import System.Environment (getEnv)
import Telegram.Bot.FSAfe.Start.Internal (tryAdvanceState, startBotGeneric)

getEnvToken :: String -> IO Tg.Token
getEnvToken = fmap fromString . getEnv

startBot_ :: (HasState a fsa ts)
          => HList' fsa BotM -> a -> Tg.Token -> IO ()
startBot_ fsa = hoistStartBot_ fsa id

startBot :: (HasState a fsa ts)
         => HList' fsa BotM -> a -> Tg.Token -> IO (Either ClientError ())
startBot fsa = hoistStartBot fsa id

hoistStartBot_
  :: forall fsa a m ts. (HasState a fsa ts)
  => HList' fsa m -> (forall x. m x -> BotM x) -> a -> Tg.Token -> IO ()
hoistStartBot_ fsa nt state token = void $ hoistStartBot fsa nt state token

hoistStartBot
  :: forall fsa a m ts. (HasState a fsa ts)
  => HList' fsa m -> (forall x. m x -> BotM x) -> a -> Tg.Token -> IO (Either ClientError ())
hoistStartBot fsa nt = startBotGeneric updateState . SomeState fsa
  where
  updateState botUser state update =
    runBotM (tryAdvanceState nt state) (BotContext botUser update)
--
-- startKeyedBot_
--   :: forall a key. (Hashable key, IsState a BotM, Aboba a BotM (OutgoingTransitions a))
--   => (Tg.Update -> Maybe key)
--   -> a
--   -> Tg.Token
--   -> IO ()
-- startKeyedBot_ = hoistStartKeyedBot_ id
--
-- startKeyedBot
--   :: forall a key. (Hashable key, IsState a BotM, Aboba a BotM (OutgoingTransitions a))
--   => (Tg.Update -> Maybe key)
--   -> a
--   -> Tg.Token
--   -> IO (Either ClientError ())
-- startKeyedBot = hoistStartKeyedBot id

hoistStartKeyedBot_
  :: forall fsa a key m ts. (Hashable key, HasState a fsa ts)
  => HList' fsa m
  -> (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO ()
hoistStartKeyedBot_ fsa nt toKey initialState token = void $
  hoistStartKeyedBot fsa nt toKey initialState token

hoistStartKeyedBot
  :: forall fsa a key m ts. (Hashable key, HasState a fsa ts)
  => HList' fsa m
  -> (forall x. m x -> BotM x)
  -> (Tg.Update -> Maybe key)
  -> a
  -> Tg.Token
  -> IO (Either ClientError ())
hoistStartKeyedBot fsa nt toKey initialState = startBotGeneric updateStateMap HM.empty
  where
  updateStateMap botUser stateMap update = do
    let key = toKey update
    let defState = SomeState fsa initialState
    let state = stateMap & HM.lookupDefault defState key
    runBotM (tryAdvanceState nt state) (BotContext botUser update) >>= \case
      Just nextState -> return $ Just $ stateMap & HM.insert key nextState
      Nothing -> return $ Just stateMap

