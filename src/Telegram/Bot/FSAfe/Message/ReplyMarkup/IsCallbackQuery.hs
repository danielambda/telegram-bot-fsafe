module Telegram.Bot.FSAfe.Message.ReplyMarkup.IsCallbackQuery where

import qualified Data.Text as T (Text, pack, unpack)
import Text.Read (readMaybe)
import Telegram.Bot.API (InlineKeyboardButton (..), labeledInlineKeyboardButton)

callbackButton :: IsCallbackQuery a => T.Text -> a -> InlineKeyboardButton
callbackButton label callback = (labeledInlineKeyboardButton label)
  {inlineKeyboardButtonCallbackData = Just (toCallbackQueryData callback)}

class IsCallbackQuery a where
  toCallbackQueryData :: a -> T.Text
  fromCallbackQueryData :: T.Text -> Maybe a

newtype ReadShow a = ReadShow a
  deriving (Read, Show, Eq)

instance (Read a, Show a) => IsCallbackQuery (ReadShow a) where
  toCallbackQueryData = T.pack . show
  fromCallbackQueryData = readMaybe . T.unpack

instance IsCallbackQuery T.Text where
  toCallbackQueryData = id
  fromCallbackQueryData = pure

instance IsCallbackQuery String where
  toCallbackQueryData = T.pack
  fromCallbackQueryData = pure . T.unpack
