module Telegram.Bot.FSAfe.Message where

import Data.Text (Text)
import Telegram.Bot.API (LinkPreviewOptions, ParseMode, SomeReplyMarkup)

data Message = Message
  { messageText :: Text
  , messageReplyMarkup :: Maybe SomeReplyMarkup
  , messageParseMode :: Maybe ParseMode
  , messageLinkPreviewOptions :: Maybe LinkPreviewOptions
  }

textMessage :: Text -> Message
textMessage text = Message text Nothing Nothing Nothing

data MessageShowMode
  = NoMessage
  | Send Message
  | Edit Message


