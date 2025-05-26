module Telegram.Bot.FSAfe.Message.ReplyMarkup.IsCallbackQuery
  ( button, buttonWith
  , IsCallbackQuery(..), IsCallbackQueryWith(..)
  ) where

import qualified Data.Text as T (Text, pack, unpack)
import Text.Read (readMaybe)
import Telegram.Bot.API (InlineKeyboardButton (..), labeledInlineKeyboardButton)
import Telegram.Bot.FSAfe.Utils (ReadShow(..))

button :: IsCallbackQuery a => T.Text -> a -> InlineKeyboardButton
button label = button' label . toCallbackQueryData

buttonWith :: IsCallbackQueryWith r a => r -> T.Text -> a -> InlineKeyboardButton
buttonWith r label = button' label . toCallbackQueryDataWith r

button' :: T.Text -> T.Text -> InlineKeyboardButton
button' label callback = (labeledInlineKeyboardButton label)
  {inlineKeyboardButtonCallbackData = Just callback}

class IsCallbackQuery a where
  toCallbackQueryData :: a -> T.Text
  fromCallbackQueryData :: T.Text -> Maybe a

instance {-# OVERLAPPABLE #-} IsCallbackQuery a => IsCallbackQueryWith r a where
  toCallbackQueryDataWith _ = toCallbackQueryData
  fromCallbackQueryDataWith _ = fromCallbackQueryData

class IsCallbackQueryWith r a where
  toCallbackQueryDataWith :: r -> a -> T.Text
  fromCallbackQueryDataWith :: r -> T.Text -> Maybe a

instance (Read a, Show a) => IsCallbackQuery (ReadShow a) where
  toCallbackQueryData = T.pack . show . unReadShow
  fromCallbackQueryData = fmap ReadShow . readMaybe . T.unpack

instance IsCallbackQuery T.Text where
  toCallbackQueryData = id
  fromCallbackQueryData = pure

instance IsCallbackQuery String where
  toCallbackQueryData = T.pack
  fromCallbackQueryData = pure . T.unpack
