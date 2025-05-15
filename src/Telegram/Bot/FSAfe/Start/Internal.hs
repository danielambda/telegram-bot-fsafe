{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/BotApp/Internal.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module Telegram.Bot.FSAfe.Start.Internal
  ( tryAdvanceState
  , startBotGeneric
  ) where

import Data.Aeson.Types (parseEither, FromJSON (parseJSON))
import qualified Telegram.Bot.API as Tg
import Servant.Client (ClientError, ClientM, runClientM)

import Telegram.Bot.FSAfe.FSA
  (IsState (..), MessageContext (..), SomeStateFrom (..), HasState (..))
import Telegram.Bot.FSAfe.BotM (BotM)
import Control.Monad.Error.Class (catchError)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (forever)
import Control.Monad.Reader (ask)
import Control.Concurrent (threadDelay, killThread)
import Control.Concurrent.STM
    ( atomically,
      newTVarIO, readTVarIO, writeTVar,
      newTQueueIO, readTQueue, writeTQueue )
import Data.Either (partitionEithers)
import Data.List.NonEmpty (NonEmpty, nonEmpty)
import Control.Concurrent.Async (Async(asyncThreadId), async, link)
import Telegram.Bot.DSL (renderMessage, HasTaggedContext (getTaggedContext), (.++))
import Data.Proxy (Proxy(..))
import Telegram.Bot.FSAfe.Reply (reply, toReplyMessage)

tryAdvanceState
  :: forall fsa m.
     Proxy fsa
  -> (forall x. m x -> BotM x)
  -> SomeStateFrom fsa m
  -> BotM (SomeStateFrom fsa m)
tryAdvanceState _ nt (SomeStateFrom (state :: state)) = do
  botCtx <- ask
  SomeStateFrom (state' :: to) <- nt $ handleState @state @fsa botCtx state
  nt (extractMessageContext state') >>= \case
    MessageContext extractedCtx -> do
      let stateCtx = getTaggedContext state'
      let ctx = extractedCtx .++ stateCtx
      let msg = renderMessage (Proxy @(StateMessage to)) ctx
      reply $ toReplyMessage msg
  return $ SomeStateFrom state'
  -- case parseTransitions @fsa state botCtx of
  --   Nothing -> pure $ SomeStateData state
  --   Just (SomeTransition transition) -> do
  --     (state' :: to) <- nt $ handleTransition transition state
  --     nt (extractMessageContext state') >>= \case
  --       MessageContext extractedCtx -> do
  --         let stateCtx = getTaggedContext state'
  --         let ctx = extractedCtx .++ stateCtx
  --         let msg = renderMessage (Proxy @(StateMessage to)) ctx
  --         reply $ toReplyMessage msg
  --     return $ SomeStateData state'

startBotGeneric
  :: (Tg.User -> state -> Tg.Update -> ClientM (Maybe state))
  -> state
  -> Tg.Token
  -> IO (Either ClientError ())
startBotGeneric mkUpdateState initialState token = do
  clientEnv <- Tg.defaultTelegramClientEnv token
  botUser <- either (error . show) Tg.responseResult <$> runClientM Tg.getMe clientEnv
  let updateState = mkUpdateState botUser
  stateTVar <- newTVarIO initialState
  updatesQueue <- newTQueueIO
  startPollingAndProcessingThreads clientEnv updateState stateTVar updatesQueue
  where
  startPollingAndProcessingThreads clientEnv updateState stateTVar updatesQueue = do
    processingThread <- processUpdatesIndefinetely
    result <- runClientM startBotPolling clientEnv
    killThread processingThread
    return result
    where
    processUpdatesIndefinetely = fmap asyncThreadId . asyncLink . forever $ do
      update <- atomically $ readTQueue updatesQueue
      currentState <- readTVarIO stateTVar
      runClientM (updateState currentState update) clientEnv >>=
        either
          (putStrLn . ("Error processing update: " ++) . show)
          (maybe (pure ()) (atomically . writeTVar stateTVar))

    asyncLink action = do
      a <- async action
      link a
      return a

    startBotPolling = startPolling $ liftIO . atomically . writeTQueue updatesQueue

startPolling :: (Tg.Update -> ClientM a) -> ClientM any
startPolling handleUpdate = go Nothing
  where
  go mLastUpdateId = do
    let offset = increment <$> mLastUpdateId
    res <- (Right <$>
              Tg.getUpdatesAsValue (Tg.GetUpdatesRequest offset Nothing (Just 25) Nothing)
           ) `catchError` (pure . Left)
    mNextUpdateId <- case res of
      Left servantErr -> do
        liftIO $ print servantErr
        return mLastUpdateId
      Right result -> do
        let (errors, updates) = parseUpdates $ Tg.responseResult result
        mapM_ reportParseError errors
        mapM_ handleUpdate updates
        return $ maximumOn1 Tg.updateUpdateId <$> nonEmpty updates
    liftIO $ threadDelay 1000000
    go mNextUpdateId

  increment (Tg.UpdateId n) = Tg.UpdateId (n + 1)

  maximumOn1 :: Ord b => (a -> b) -> NonEmpty a -> b
  maximumOn1 f = maximum . fmap f

  parseUpdates = partitionEithers . map (parseEither parseJSON)

  reportParseError err = liftIO $ putStrLn $
    "Failed to parse an update! Please, make sure you have the latest version of `telegram-bot-api`\
    \ library and consider opening an issue if so. Error message: " <> err
