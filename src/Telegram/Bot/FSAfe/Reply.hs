{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/Reply.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.FSAfe.Reply where

import Data.Text (Text)
import Data.Text as T (pack)
import Telegram.Bot.API hiding (Message, editMessageText, editMessageReplyMarkup)

import Control.Applicative ((<|>))
import Control.Monad (void, (<=<))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (asks)
import GHC.Generics (Generic)

import Telegram.Bot.DSL.Message (Message(..), textMessage)

import Telegram.Bot.FSAfe.RunTG (runTG)
import Telegram.Bot.FSAfe.BotM (BotContext(..), MonadBot (..))

currentChatId :: MonadBot m => m (Maybe ChatId)
currentChatId = liftBot $ asks $ updateChatId . botContextUpdate

getEditMessageId :: MonadBot m => m (Maybe EditMessageId)
getEditMessageId = liftBot $ asks $ updateEditMessageId . botContextUpdate

updateEditMessageId :: Update -> Maybe EditMessageId
updateEditMessageId update
    = EditInlineMessageId <$> (callbackQueryInlineMessageId =<< updateCallbackQuery update)
  <|> (EditChatMessageId . SomeChatId . chatId . messageChat <$> message)
      <*> (messageMessageId <$> message)
  where message = extractUpdateMessage update

-- | Reply message parameters.
-- This is just like 'SendMessageRequest' but without 'SomeChatId' specified.
data ReplyMessage = ReplyMessage
  { replyMessageText                  :: Text -- ^ Text of the message to be sent.
  , replyMessageMessageThreadId       :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , replyMessageParseMode             :: Maybe ParseMode -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , replyMessageEntities              :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in message text, which can be specified instead of /parse_mode/.
  , replyMessageLinkPreviewOptions    :: Maybe LinkPreviewOptions -- ^ Link preview generation options for the message.
  , replyMessageDisableNotification   :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , replyMessageProtectContent        :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , replyMessageReplyToMessageId      :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  , replyMessageReplyParameters       :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , replyMessageReplyMarkup           :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  } deriving (Generic)

toReplyMessage :: Message -> ReplyMessage
toReplyMessage Message{..}
  = ReplyMessage messageText
  Nothing messageParseMode
  Nothing messageLinkPreviewOptions
  Nothing Nothing Nothing
  Nothing messageReplyMarkup

replyMessageToSendMessageRequest :: SomeChatId -> ReplyMessage -> SendMessageRequest
replyMessageToSendMessageRequest someChatId ReplyMessage{..} = SendMessageRequest
  { sendMessageChatId = someChatId
  , sendMessageBusinessConnectionId = Nothing
  , sendMessageMessageEffectId = Nothing
  , sendMessageMessageThreadId = replyMessageMessageThreadId
  , sendMessageText = replyMessageText
  , sendMessageParseMode = replyMessageParseMode
  , sendMessageEntities = replyMessageEntities
  , sendMessageLinkPreviewOptions = replyMessageLinkPreviewOptions
  , sendMessageDisableNotification = replyMessageDisableNotification
  , sendMessageProtectContent = replyMessageProtectContent
  , sendMessageReplyToMessageId = replyMessageReplyToMessageId
  , sendMessageReplyMarkup = replyMessageReplyMarkup
  , sendMessageReplyParameters = replyMessageReplyParameters
  }

replyTo :: MonadBot m => SomeChatId -> ReplyMessage -> m ()
replyTo someChatId rmsg = do
  let msg = replyMessageToSendMessageRequest someChatId rmsg
  void $ runTG msg

reply :: MonadBot m => ReplyMessage -> m ()
reply rmsg = do
  mchatId <- liftBot currentChatId
  case mchatId of
    Just chatId -> replyTo (SomeChatId chatId) rmsg
    Nothing     -> liftIO $ putStrLn "No chat to reply to"

replyText :: MonadBot m => Text -> m ()
replyText = reply . toReplyMessage . textMessage

data EditMessage = EditMessage
  { editMessageText                  :: Text
  , editMessageParseMode             :: Maybe ParseMode
  , editMessageLinkPreviewOptions    :: Maybe LinkPreviewOptions
  , editMessageReplyMarkup           :: Maybe SomeReplyMarkup
  }

data EditMessageId
  = EditChatMessageId SomeChatId MessageId
  | EditInlineMessageId InlineMessageId

toEditMessage :: Text -> EditMessage
toEditMessage msg = EditMessage msg Nothing Nothing Nothing

editMessageToEditMessageTextRequest
  :: EditMessageId -> EditMessage -> EditMessageTextRequest
editMessageToEditMessageTextRequest editMessageId EditMessage{..}
  = EditMessageTextRequest
    { editMessageTextText = editMessageText
    , editMessageTextParseMode = editMessageParseMode
    , editMessageTextLinkPreviewOptions = editMessageLinkPreviewOptions
    , editMessageTextReplyMarkup = editMessageReplyMarkup
    , editMessageEntities = Nothing
    , ..
    }
  where
    ( editMessageTextChatId,
      editMessageTextMessageId,
      editMessageTextInlineMessageId )
      = case editMessageId of
          EditChatMessageId chatId messageId
            -> (Just chatId, Just messageId, Nothing)
          EditInlineMessageId messageId
            -> (Nothing, Nothing, Just messageId)

editMessageToReplyMessage :: EditMessage -> ReplyMessage
editMessageToReplyMessage EditMessage{..} = (toReplyMessage $ textMessage editMessageText)
  { replyMessageParseMode = editMessageParseMode
  , replyMessageLinkPreviewOptions = editMessageLinkPreviewOptions
  , replyMessageReplyMarkup = editMessageReplyMarkup
  }

editMessage :: MonadBot m => EditMessageId -> EditMessage -> m ()
editMessage editMessageId emsg = do
  let msg = editMessageToEditMessageTextRequest editMessageId emsg
  void $ runTG msg

editUpdateMessage :: MonadBot m => EditMessage -> m ()
editUpdateMessage emsg = do
  mEditMessageId <- getEditMessageId
  case mEditMessageId of
    Just editMessageId -> editMessage editMessageId emsg
    Nothing            -> liftIO $ putStrLn "Can't find message to edit!"

editUpdateMessageText :: MonadBot m => Text -> m ()
editUpdateMessageText = editUpdateMessage . toEditMessage

replyOrEdit :: MonadBot m => EditMessage -> m ()
replyOrEdit emsg = do
  uid <- liftBot $ asks $ fmap userId . (messageFrom <=< extractUpdateMessage . botContextUpdate)
  botUserId <- liftBot $ asks $ userId . botContextUser
  if uid == Just botUserId
     then editUpdateMessage emsg
     else reply (editMessageToReplyMessage emsg)
