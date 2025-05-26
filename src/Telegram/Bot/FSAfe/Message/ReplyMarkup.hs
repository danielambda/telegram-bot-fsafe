module Telegram.Bot.FSAfe.Message.ReplyMarkup
  ( module Exports
  , IsInlineKeyboardMarkup(..)
  , single, col, row, grid
  ) where

import Telegram.Bot.FSAfe.Message.ReplyMarkup.IsCallbackQuery as Exports

import Telegram.Bot.API (InlineKeyboardButton, InlineKeyboardMarkup (..))

import Control.Monad (join)
import Data.Foldable (toList)

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

single :: InlineKeyboardButton -> InlineKeyboardMarkup
single btn = InlineKeyboardMarkup [[btn]]

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
