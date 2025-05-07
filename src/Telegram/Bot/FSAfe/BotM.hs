{-
  This file contains code derived from a project licensed under the BSD-3-Clause License.
  See LICENSE-BSD in the project root for the full license details.
  Original source: https://github.com/fizruk/telegram-bot-simple/blob/master/telegram-bot-simple/src/Telegram/Bot/Simple/Eff.hs

  Copyright (c) 2017-2023, Nickolay Kudasov
  All rights reserved.
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Telegram.Bot.FSAfe.BotM
  ( BotM, BotContext(..)
  , liftClientM, runBotM
  , MonadBot(..)
  ) where

import qualified Telegram.Bot.API as Tg (User, Update)
import Control.Monad.Reader (ReaderT (..), MonadReader, MonadTrans (lift))
import Servant.Client (ClientM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Except (ExceptT, runExceptT, MonadError (throwError))

-- I know i could've imported MaybeT
newtype BotM a = BotM (ReaderT BotContext (ExceptT () ClientM) a)
  deriving (Functor, Applicative, Monad, MonadReader BotContext, MonadError (), MonadIO)

instance MonadFail BotM where
  fail _ = throwError ()

data BotContext = BotContext
  { botContextUser   :: Tg.User
  , botContextUpdate :: Tg.Update
  }

runBotM :: BotM a -> BotContext -> ClientM (Maybe a)
runBotM (BotM reader) = fmap f . runExceptT . runReaderT reader
  where f (Left ()) = Nothing
        f (Right a) = Just a

class MonadIO m => MonadBot m where
  liftBot :: BotM a -> m a

instance MonadBot BotM where
  liftBot = id

liftClientM :: MonadBot m => ClientM a -> m a
liftClientM = liftBot . BotM . lift . lift

