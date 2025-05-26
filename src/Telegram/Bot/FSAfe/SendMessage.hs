{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/Reply.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE LambdaCase #-}

module Telegram.Bot.FSAfe.SendMessage
  ( getChatId, getMessageId, getMessageEditMessageId
  , answerCallbackQuery
  , send, send_
  , sendIn, sendIn_
  , sendText, sendText_
  , edit, edit_
  , editInThisChat, editInThisChat_
  , editUpdateMessage, editUpdateMessage_
  , editReplyMarkup
  , editUpdateMessageReplyMarkup ,editUpdateMessageReplyMarkup_
  ) where

import Data.Text (Text)
import Telegram.Bot.API hiding (Message, editMessageText, editMessageReplyMarkup, answerCallbackQuery)
import qualified Telegram.Bot.API as Tg (Message)

import Control.Monad (void)
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)

import Telegram.Bot.FSAfe.BotM (BotContext(..), MonadBot (..))
import Telegram.Bot.FSAfe.RunTG (runTG)
import Telegram.Bot.FSAfe.Message
  (Message(..), textMessage, EditMessageId (..), toSendMessageRequest, toEditMessageTextRequest)

answerCallbackQuery :: MonadBot m => m ()
answerCallbackQuery = liftBot $
  asks (updateCallbackQuery . botContextUpdate) >>= traverse_
    (runTG . defAnswerCallbackQuery . callbackQueryId)

getChatId :: MonadBot m => m (Maybe ChatId)
getChatId = liftBot $ asks $ updateChatId . botContextUpdate

getMessageId :: MonadBot m => m (Maybe MessageId)
getMessageId = liftBot $ asks $ fmap messageMessageId . updateMessage . botContextUpdate

getMessageEditMessageId :: MonadBot m => m (Maybe EditMessageId)
getMessageEditMessageId =
  liftA2 EditChatMessageId . fmap SomeChatId <$> getChatId <*> getMessageId

sendIn :: MonadBot m => SomeChatId -> Message -> m Tg.Message
sendIn someChatId rmsg = do
  let msg = toSendMessageRequest someChatId rmsg
  responseResult <$> runTG msg

sendIn_ :: MonadBot m => SomeChatId -> Message -> m ()
sendIn_ = fmap void . sendIn

send :: MonadBot m => Message -> m Tg.Message
send rmsg =
  liftBot getChatId >>= \case
    Just chatId -> sendIn (SomeChatId chatId) rmsg
    Nothing     -> liftBot $ fail "No chat to reply to"

send_ :: MonadBot m => Message -> m ()
send_ = void . send

sendText :: MonadBot m => Text -> m Tg.Message
sendText = send . textMessage

sendText_ :: MonadBot m => Text -> m ()
sendText_ = void . sendText

edit :: MonadBot m => EditMessageId -> Message -> m EditMessageResponse
edit emsgId =
  fmap responseResult . runTG . toEditMessageTextRequest emsgId

edit_ :: MonadBot m => EditMessageId -> Message -> m ()
edit_ = fmap void . edit

editInThisChat :: MonadBot m => MessageId -> Message -> m EditMessageResponse
editInThisChat msgId msg =
  liftBot getChatId >>= \case
    Just chatId -> edit (EditChatMessageId (SomeChatId chatId) msgId) msg
    Nothing     -> liftBot $ fail "No chat to reply to"

editInThisChat_ :: MonadBot f => MessageId -> Message -> f ()
editInThisChat_ msgId msg = void $ editInThisChat msgId msg

editUpdateMessage :: MonadBot m => Message -> m EditMessageResponse
editUpdateMessage msg =
  getMessageId >>= \case
    Just msgId -> editInThisChat msgId msg
    Nothing    -> liftBot $ fail "No message to edit"

editUpdateMessage_ :: MonadBot m => Message -> m ()
editUpdateMessage_ = void . editUpdateMessage_

editUpdateMessageReplyMarkup :: MonadBot m => SomeReplyMarkup -> m EditMessageResponse
editUpdateMessageReplyMarkup replyMarkup = do
  getMessageEditMessageId >>= \case
    Just editMsgId -> editReplyMarkup editMsgId replyMarkup
    Nothing -> liftBot $ fail "No message to edit"

editUpdateMessageReplyMarkup_ :: MonadBot m => SomeReplyMarkup -> m ()
editUpdateMessageReplyMarkup_ = void . editUpdateMessageReplyMarkup

editReplyMarkup :: MonadBot m => EditMessageId -> SomeReplyMarkup -> m EditMessageResponse
editReplyMarkup editMessageId replyMarkup =
  fmap responseResult $ runTG $ case editMessageId of
    EditChatMessageId chatId msgId ->
      EditMessageReplyMarkupRequest (Just chatId) (Just msgId) Nothing            (Just replyMarkup)
    EditInlineMessageId inlineMsgId ->
      EditMessageReplyMarkupRequest Nothing       Nothing      (Just inlineMsgId) (Just replyMarkup)
