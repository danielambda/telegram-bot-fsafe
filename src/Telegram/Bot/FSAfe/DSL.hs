{-# LANGUAGE TypeFamilies, StandaloneKindSignatures, DataKinds, TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Bot.FSAfe.DSL where

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (SSymbol, KnownSymbol, symbolVal)
import Telegram.Bot.FSAfe.Reply (ReplyMessage(..), toReplyMessage)
import qualified Data.Text as T
import Telegram.Bot.API (InlineKeyboardButton, InlineKeyboardMarkup (..))
import Data.List.NonEmpty (NonEmpty(..))

import Telegram.Bot.FSAfe.UndecidableInstances

type Btn' text callback = Btn (TxtLn (SSymbol text :|'[])) callback

type MyMessage
  =  MyTextMessage
  \| MyKeyboard

type MyTextMessage
  =  "Hello, my dear " :|: Var "name" :|: "!"
  \| "There are no buttons below"

type MyKeyboard
  =  Btn' "" '[]            :#: Btn ("My name is not" :|: Var "name") '[]
  \| Btn' "Hello world" '[] :#: Btn' "Aboba" '[]

myMessage :: ReplyMessage
myMessage =
  let msgData = ((T.pack "Daniel", ()), ())
  in fromMessageData (Proxy @MyTextMessage) msgData

type IsMessage :: Message -> Constraint
class IsMessage a where
  type MessageData a :: Type
  fromMessageData :: Proxy a -> MessageData a -> ReplyMessage

instance IsTextLine (TxtLn l)
      => IsMessage (Msg (TxtLn l :| '[]) 'Nothing) where
  type MessageData (Msg (TxtLn l :| '[]) 'Nothing) = TextLineData (TxtLn l)
  fromMessageData _ tlData = toReplyMessage $
    fromTextLineData (Proxy @(TxtLn l)) tlData

instance (IsTextLine (TxtLn l), IsMessage (Msg (l1 :| ls) kb))
      => IsMessage (Msg (TxtLn l :| l1 : ls) kb) where
  type MessageData (Msg (TxtLn l :| l1 : ls) kb) =
    (TextLineData (TxtLn l), MessageData (Msg (l1 :| ls) kb))
  fromMessageData _ (tlData, msgData) = let
    newLine = fromTextLineData (Proxy @(TxtLn l)) tlData
    replyMessage = fromMessageData (Proxy @(Msg (l1 :| ls) kb)) msgData
    initialText = replyMessageText replyMessage
    newText = T.unlines [newLine, initialText]
    replyMessage' = replyMessage{replyMessageText = newText}
    in replyMessage'

type IsTextLine :: TextLine -> Constraint
class IsTextLine a where
  type TextLineData a :: Type
  fromTextLineData :: Proxy a -> TextLineData a -> T.Text

instance KnownSymbol s
      => IsTextLine (TxtLn (SSymbol s :| '[])) where
  type TextLineData (TxtLn (SSymbol s :| '[])) = ()
  fromTextLineData _ _ = T.pack $ symbolVal $ Proxy @s

instance IsTextLine (TxtLn (Var a :| '[])) where
  type TextLineData (TxtLn (Var a :| '[])) = T.Text
  fromTextLineData _ var = var

instance IsTextLine (TxtLn (Var' :| '[])) where
  type TextLineData (TxtLn (Var' :| '[])) = T.Text
  fromTextLineData _ = id

instance (KnownSymbol s, IsTextLine (TxtLn (l :| ls)))
      => IsTextLine (TxtLn (SSymbol s :| l : ls)) where
  type TextLineData (TxtLn (SSymbol s :| l : ls)) = TextLineData (TxtLn (l :| ls))
  fromTextLineData _ tlData
    =  T.pack (symbolVal $ Proxy @s)
    <> fromTextLineData (Proxy @(TxtLn (l:|ls))) tlData

instance IsTextLine (TxtLn (l :| ls))
      => IsTextLine (TxtLn (Var a :| l : ls)) where
  type TextLineData (TxtLn (Var a :| l : ls)) = (T.Text, TextLineData (TxtLn (l :| ls)))
  fromTextLineData _ (var, tlData)
    =  var
    <> fromTextLineData (Proxy @(TxtLn (l:|ls))) tlData

type IsKeyboard :: [[Type]] -> Constraint
class IsKeyboard a where
  type KeyboardData a :: Type
  fromKeyboardData :: Proxy a -> KeyboardData a -> InlineKeyboardMarkup

instance IsKeyboard '[] where
  type KeyboardData '[] = ()
  fromKeyboardData _ _ = InlineKeyboardMarkup []

instance (IsButtonLine x, IsKeyboard xs)
      => IsKeyboard (x ': xs) where
  type KeyboardData (x ': xs) = (ButtonLineData x, KeyboardData xs)
  fromKeyboardData _ (x, xs) = let
    hd = fromButtonLineData (Proxy @x) x
    InlineKeyboardMarkup tl = fromKeyboardData (Proxy @xs) xs
    in InlineKeyboardMarkup (hd : tl)

type IsButtonLine :: [Type] -> Constraint
class IsButtonLine a where
  type ButtonLineData a :: Type
  fromButtonLineData :: Proxy a -> ButtonLineData a -> [InlineKeyboardButton]
