module Telegram.Bot.FSAfe.Message where

import Data.Text (Text)

import Telegram.Bot.API (LinkPreviewOptions, ParseMode, SomeReplyMarkup (..), MessageEntity)
import Telegram.Bot.FSAfe.Message.ReplyMarkup (IsInlineKeyboardMarkup (..))

data Message = Message
  { messageText :: Text
  , messageReplyMarkup :: Maybe SomeReplyMarkup
  , messageParseMode :: Maybe ParseMode
  , messageLinkPreviewOptions :: Maybe LinkPreviewOptions
  , messageEntities :: Maybe [MessageEntity]
  }

data MessageShowMode
  = NoMessage
  | Send Message
  | Edit Message

textMessage :: Text -> Message
textMessage text = Message text Nothing Nothing Nothing Nothing

withInlineKeyboard :: IsInlineKeyboardMarkup a => a -> Message -> Message
withInlineKeyboard keyboard msg =
  msg{messageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ toInlineKeyboardMarkup keyboard}

