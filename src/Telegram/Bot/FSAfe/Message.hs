module Telegram.Bot.FSAfe.Message where

import Data.Text (Text)
import Data.Foldable (toList)
import Telegram.Bot.API (LinkPreviewOptions, ParseMode, SomeReplyMarkup (..), MessageEntity, InlineKeyboardButton, InlineKeyboardMarkup (InlineKeyboardMarkup, inlineKeyboardMarkupInlineKeyboard))
import Control.Monad (join)

data Message = Message
  { messageText :: Text
  , messageReplyMarkup :: Maybe SomeReplyMarkup
  , messageParseMode :: Maybe ParseMode
  , messageLinkPreviewOptions :: Maybe LinkPreviewOptions
  , messageEntities :: Maybe [MessageEntity]
  }

textMessage :: Text -> Message
textMessage text = Message text Nothing Nothing Nothing Nothing

data MessageShowMode
  = NoMessage
  | Send Message
  | Edit Message

class IsInlineKeyboardMarkup a where
  toInlineKeyboardMarkup :: a -> InlineKeyboardMarkup

instance IsInlineKeyboardMarkup InlineKeyboardMarkup where
  toInlineKeyboardMarkup = id

instance (Foldable f, IsInlineKeyboardMarkup a) => IsInlineKeyboardMarkup (f a) where
  toInlineKeyboardMarkup
    = InlineKeyboardMarkup
    . join
    . map (inlineKeyboardMarkupInlineKeyboard . toInlineKeyboardMarkup)
    . toList

withInlineKeyboard :: IsInlineKeyboardMarkup a => a -> Message -> Message
withInlineKeyboard keyboard msg =
  msg{messageReplyMarkup = Just $ SomeInlineKeyboardMarkup $ toInlineKeyboardMarkup keyboard}

single :: InlineKeyboardButton -> InlineKeyboardMarkup
single button = InlineKeyboardMarkup [[button]]

col :: Foldable t => t InlineKeyboardButton -> InlineKeyboardMarkup
col = InlineKeyboardMarkup . map (:[]) . toList

row :: Foldable f => f InlineKeyboardButton -> InlineKeyboardMarkup
row = InlineKeyboardMarkup . (:[]) . toList

grid :: Foldable t => Int -> t InlineKeyboardButton -> InlineKeyboardMarkup
grid w = InlineKeyboardMarkup . chunksOfW . toList
  where
    chunksOfW [] = []
    chunksOfW xs
      | w <= 0 = [xs]
      | otherwise = chunk : chunksOfW rest
          where (chunk, rest) = splitAt w xs
