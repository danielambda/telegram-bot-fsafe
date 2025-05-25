{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/Reply.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE RecordWildCards #-}

module Telegram.Bot.FSAfe.Message
  ( module Exports
  , Message(..)
  , ShowMode(..)
  , EditMessageId(..)
  , textMessage
  , toSendMessageRequest, toEditMessageTextRequest
  , withInlineKeyboard, withParseMode
  ) where

import Telegram.Bot.FSAfe.Message.ReplyMarkup as Exports

import Data.Text (Text)
import Telegram.Bot.API
  ( LinkPreviewOptions, ParseMode, SomeReplyMarkup (..), MessageEntity
  , MessageId, ReplyParameters, MessageThreadId, BusinessConnectionId
  , SomeChatId, SendMessageRequest (..), InlineMessageId, EditMessageTextRequest (..)
  )

import GHC.Generics (Generic)

-- | Message parameters.
-- This is just like 'SendMessageRequest' but without 'SomeChatId' specified.
data Message
  = Message
  { messageText                 :: Text -- ^ Text of the message to be sent.
  , messageBusinessConnectionId :: Maybe BusinessConnectionId -- ^ Unique identifier of the business connection on behalf of which the message will be sent.
  , messageDisableNotification  :: Maybe Bool -- ^ Sends the message silently. Users will receive a notification with no sound.
  , messageEntities             :: Maybe [MessageEntity] -- ^ A JSON-serialized list of special entities that appear in message text, which can be specified instead of /parse_mode/.
  , messageLinkPreviewOptions   :: Maybe LinkPreviewOptions -- ^ Link preview generation options for the message.
  , messageMessageEffectId      :: Maybe Text -- ^ Unique identifier of the message effect to be added to the message; for private chats only.
  , messageMessageThreadId      :: Maybe MessageThreadId -- ^ Unique identifier for the target message thread (topic) of the forum; for forum supergroups only.
  , messageParseMode            :: Maybe ParseMode -- ^ Send 'MarkdownV2', 'HTML' or 'Markdown' (legacy), if you want Telegram apps to show bold, italic, fixed-width text or inline URLs in your bot's message.
  , messageProtectContent       :: Maybe Bool -- ^ Protects the contents of the sent message from forwarding and saving.
  , messageReplyMarkup          :: Maybe SomeReplyMarkup -- ^ Additional interface options. A JSON-serialized object for an inline keyboard, custom reply keyboard, instructions to remove reply keyboard or to force a reply from the user.
  , messageReplyParameters      :: Maybe ReplyParameters -- ^ Description of the message to reply to.
  , messageReplyToMessageId     :: Maybe MessageId -- ^ If the message is a reply, ID of the original message.
  } deriving (Generic)

data ShowMode
  = NoMessage
  | Send Message
  | Edit Message

data EditMessageId
  = EditChatMessageId SomeChatId MessageId
  | EditInlineMessageId InlineMessageId

textMessage :: Text -> Message
textMessage text = Message text
  Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

toSendMessageRequest :: SomeChatId -> Message -> SendMessageRequest
toSendMessageRequest someChatId Message{..}
  = SendMessageRequest
  { sendMessageChatId = someChatId
  , sendMessageBusinessConnectionId = messageBusinessConnectionId
  , sendMessageDisableNotification = messageDisableNotification
  , sendMessageEntities = messageEntities
  , sendMessageLinkPreviewOptions = messageLinkPreviewOptions
  , sendMessageMessageEffectId = messageMessageEffectId
  , sendMessageMessageThreadId = messageMessageThreadId
  , sendMessageParseMode = messageParseMode
  , sendMessageProtectContent = messageProtectContent
  , sendMessageReplyMarkup = messageReplyMarkup
  , sendMessageReplyParameters = messageReplyParameters
  , sendMessageReplyToMessageId = messageReplyToMessageId
  , sendMessageText = messageText
  }

toEditMessageTextRequest :: EditMessageId -> Message -> EditMessageTextRequest
toEditMessageTextRequest editMessageId Message{..}
  = EditMessageTextRequest
  { editMessageTextText = messageText
  , editMessageTextParseMode = messageParseMode
  , editMessageTextLinkPreviewOptions = messageLinkPreviewOptions
  , editMessageTextReplyMarkup = messageReplyMarkup
  , editMessageEntities = messageEntities
  , ..
  }
  where
    ( editMessageTextChatId,
      editMessageTextMessageId,
      editMessageTextInlineMessageId )
      = case editMessageId of
          EditChatMessageId chatId messageId ->
            (Just chatId, Just messageId, Nothing)
          EditInlineMessageId messageId ->
            (Nothing, Nothing, Just messageId)

withInlineKeyboard :: IsInlineKeyboardMarkup a => a -> Message -> Message
withInlineKeyboard keyboard msg =
  msg{messageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ toInlineKeyboardMarkup keyboard}

withParseMode :: ParseMode -> Message -> Message
withParseMode parseMode msg =
  msg{messageParseMode = Just parseMode}
