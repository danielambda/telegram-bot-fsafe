{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/Reply.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

module Telegram.Bot.FSAfe.SendMessage
  ( answerCallbackQuery
  , send, send_
  , sendIn, sendIn_
  , sendText, sendText_
  , edit, edit_
  ) where

import Data.Text (Text)
import Telegram.Bot.API hiding (Message, editMessageText, editMessageReplyMarkup, answerCallbackQuery)
import qualified Telegram.Bot.API as Tg (Message)

import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import Data.Foldable (traverse_)

import Telegram.Bot.FSAfe.BotM (BotContext(..), MonadBot (..))
import Telegram.Bot.FSAfe.RunTG (runTG)
import Telegram.Bot.FSAfe.Message (Message(..), textMessage, EditMessageId, toSendMessageRequest, toEditMessageTextRequest)
import Control.Monad (void)

answerCallbackQuery :: MonadBot m => m ()
answerCallbackQuery = liftBot $
  asks (updateCallbackQuery . botContextUpdate) >>= traverse_
    (runTG . defAnswerCallbackQuery . callbackQueryId)

currentChatId :: MonadBot m => m (Maybe ChatId)
currentChatId = liftBot $ asks $ updateChatId . botContextUpdate

sendIn :: MonadBot m => SomeChatId -> Message -> m Tg.Message
sendIn someChatId rmsg = do
  let msg = toSendMessageRequest someChatId rmsg
  responseResult <$> runTG msg

sendIn_ :: MonadBot m => SomeChatId -> Message -> m ()
sendIn_ = fmap void . sendIn

send :: MonadBot m => Message -> m Tg.Message
send rmsg = do
  mchatId <- liftBot currentChatId
  case mchatId of
    Just chatId -> sendIn (SomeChatId chatId) rmsg
    Nothing     -> liftIO $ putStrLn "No chat to reply to" >> pure undefined

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
