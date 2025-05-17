{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/UpdateParser.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE OverloadedStrings #-}

module Telegram.Bot.FSAfe.BotContextParser
  ( BotContextParser, mkBotContextParser, runBotContextParser
  , text, plainText
  , command
  , callbackQueryDataRead
  ) where

import qualified Data.Text as T
import Data.Text (Text)
import Telegram.Bot.API
  (Update(..), Message(..), extractUpdateMessage, CallbackQuery(..), User (..))

import Control.Monad ((<=<))
import Control.Monad.Reader (ReaderT(..), asks)
import Data.Char (isSpace)
import Telegram.Bot.FSAfe.BotM (BotContext (..))
import Telegram.Bot.DSL (IsCallbackData (fromCallbackData))

type BotContextParser a = ReaderT BotContext Maybe a

mkBotContextParser :: (BotContext -> Maybe a) -> BotContextParser a
mkBotContextParser = ReaderT

runBotContextParser :: BotContextParser a -> BotContext -> Maybe a
runBotContextParser = runReaderT

text :: BotContextParser Text
text = mkBotContextParser $ messageText <=< extractUpdateMessage . botContextUpdate

plainText :: BotContextParser Text
plainText = do
  t <- text
  if "/" `T.isPrefixOf` t
    then fail "command"
    else pure t

command :: Text -> BotContextParser Text
command commandName = do
  (cmd, rest) <- T.break isSpace <$> text
  mBotName <- asks $ userUsername . botContextUser
  let allowedCmds = case mBotName of
        Nothing ->      [ "/" <> commandName ]
        Just botName -> [ "/" <> commandName
                        , "/" <> commandName <> "@" <> botName
                        ]
  if cmd `elem` allowedCmds
    then pure $ T.stripStart rest
    else fail "not that command"

-- | Obtain 'CallbackQuery' @data@ associated with the callback button in an inline keyboard if present in 'Update' message.
callbackQueryDataRead :: IsCallbackData a => BotContextParser a
callbackQueryDataRead = mkBotContextParser
  $   fromCallbackData
  <=< callbackQueryData
  <=< updateCallbackQuery
  .   botContextUpdate
