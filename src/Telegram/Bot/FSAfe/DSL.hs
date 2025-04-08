{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Telegram.Bot.FSAfe.DSL (mmyMessage, MyMessage) where

import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal, TypeError, ErrorMessage(..))
import Data.Proxy (Proxy (..))
import Telegram.Bot.FSAfe (ReplyMessage (..), toReplyMessage)
import qualified Data.Text as T
import Telegram.Bot.API (InlineKeyboardButton, SomeReplyMarkup (..), InlineKeyboardMarkup (..))
import Telegram.Bot.FSAfe.Reply (callbackButton)

mmyMessage :: ReplyMessage
mmyMessage = fromMessageData (Proxy @(Proper MyMessage)) undefined

type MyMessage
  =  MyTextMessage
  \| MyKeyboard

type MyTextMessage
  =  "Hello, my dear " :|: Var "name" :|: "!"
  \| "There are 5 buttons below"
  \| Var "question"
  \| Btn "Button"

type MyKeyboard
  =  Btn ""             :|: Btn ("My name is not" :|: Var "name")
  \| Btn "Hello world"  :|: Btn "Aboba"

data ProperMessage = PMsg (NonEmpty TextLine) [ButtonLine]

newtype TextLine = TxtLn (NonEmpty TextEntity)

data TextEntity where
  Txt :: Symbol -> TextEntity
  Var :: k -> TextEntity
  Var' :: TextEntity

newtype ButtonLine = BtnLn [ButtonEntity]

data ButtonEntity where
  Btn :: k -> ButtonEntity

data Message = Msg [[TextEntity]] [[ButtonEntity]]

data MessageLine where
  MTL :: [TextEntity] -> MessageLine
  MBL :: [ButtonEntity] -> MessageLine

infixr 9 :|:
type (:|:) :: k -> l -> MessageLine
type a :|: b = JoinMessageLines (AsMessageLine a) (AsMessageLine b)

type AsMessageLine :: k -> MessageLine
type family AsMessageLine a where
  AsMessageLine (MTL a) = MTL a
  AsMessageLine (MBL a) = MBL a
  AsMessageLine (a :: Symbol) = MTL '[Txt a]
  AsMessageLine (Var a) = MTL '[Var a]
  AsMessageLine (Btn a) = MBL '[Btn a]
  AsMessageLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to MessageLine")

type JoinMessageLines :: MessageLine -> MessageLine -> MessageLine
type family JoinMessageLines ml1 ml2 where
  JoinMessageLines (MTL tl1) (MTL tl2) = MTL (tl1 ++ tl2)
  JoinMessageLines (MBL bl1) (MBL bl2) = MBL (bl1 ++ bl2)

infixl 0 \|
type (\|) :: k -> l -> Message
type a \| b = JoinMessages (AsMessage a) (AsMessage b)

type JoinMessages :: Message -> Message -> Message
type family JoinMessages m1 m2 where
  JoinMessages (Msg tls1 '[]) (Msg tls2 bls) = Msg (tls1 ++ tls2) bls
  JoinMessages (Msg tls bls1) (Msg '[] bls2) = Msg tls (bls1 ++ bls2)
  JoinMessages (Msg _ (_:_))  (Msg (_:_) _)  = TypeError (Text "Cannot have text below buttons")

type AsMessage :: k -> Message
type family AsMessage a where
  AsMessage (Msg tls bls) = Msg tls bls
  AsMessage (a :: Symbol) = Msg '[ '[Txt a]] '[]
  AsMessage (Txt a)       = Msg '[ '[Txt a]] '[]
  AsMessage (Var a)       = Msg '[ '[Var a]] '[]
  AsMessage (Btn a)       = Msg '[]          '[ '[Btn a]]
  AsMessage (MTL a)       = Msg '[a]         '[]
  AsMessage (MBL a)       = Msg '[]          '[a]
  AsMessage a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to Message")

type Proper :: Message -> ProperMessage
type family Proper msg where
  Proper (Msg '[]      _)   = TypeError (Text "Cannot have a message without text")
  Proper (Msg (tl:tls) bls) = PMsg
    (Eval (ProperTL tl) :| Eval (Map ProperTL tls))
    (Eval (Map ProperBL bls))

-- TODO move this out to some module for first-class-families styled things
type Exp a = a -> Type

type Eval :: Exp a -> a
type family Eval e

data Map :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (Map _ '[]) = '[]
type instance Eval (Map f (x:xs)) = Eval (f x) : Eval (Map f xs)
--

data ProperTL :: [TextEntity] -> Exp TextLine
type instance Eval (ProperTL '[]) = TypeError (Text "TODO")
type instance Eval (ProperTL (tl:tls)) = TxtLn (tl:|tls)

data ProperBL :: [ButtonEntity] -> Exp ButtonLine
type instance Eval (ProperBL a) = BtnLn a

infixl 1 ++
type (++) :: f k -> f k -> f k
type family as ++ bs where
  '[]      ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)
  (a :| '[]) ++ (b :| bs) = a :| b : bs
  (a :| a1 : as) ++ (b :| bs) = a :| a1 : b : (as ++ bs)

type IsMessage :: ProperMessage -> Constraint
class IsMessage a where
  type MessageData a :: Type
  fromMessageData :: Proxy a -> MessageData a -> ReplyMessage

-- induction base
instance (IsTextLine tl)
      => IsMessage (PMsg (tl :| '[]) '[]) where
  type MessageData (PMsg (tl :| '[]) '[]) = (TextLineData tl)
  fromMessageData _ tlData = let
    messageMarkup = SomeInlineKeyboardMarkup $ InlineKeyboardMarkup []
    textMessage = toReplyMessage $ fromTextLineData (Proxy @tl) tlData
    in textMessage{replyMessageReplyMarkup = Just messageMarkup}

-- induction step with text line tl
instance (IsTextLine (TxtLn tl), IsMessage (PMsg (tl1 :| tls) '[]))
      => IsMessage (PMsg (TxtLn tl :| tl1 : tls) '[]) where
  type MessageData (PMsg (TxtLn tl :| tl1 : tls) '[]) =
    (TextLineData (TxtLn tl), MessageData (PMsg (tl1 :| tls) '[]))
  fromMessageData _ (tlData, msgData) = let
    replyMessage = fromMessageData (Proxy @(PMsg (tl1 :| tls) '[])) msgData
    newText = T.unlines
            [ fromTextLineData (Proxy @(TxtLn tl)) tlData
            , replyMessageText replyMessage ]
    in replyMessage{replyMessageText = newText}

-- induction step with button line bl
instance (IsButtonLine (BtnLn bl), IsMessage (PMsg tls bls))
      => IsMessage (PMsg tls (BtnLn bl : bls)) where
  type MessageData (PMsg tls (BtnLn bl : bls)) =
    (ButtonLineData (BtnLn bl), MessageData (PMsg tls bls))
  fromMessageData _ (blData, msgData) = let
    replyMessage = fromMessageData (Proxy @(PMsg tls bls)) msgData
    buttonLine = fromButtonLineData (Proxy @(BtnLn bl)) blData
    in replyMessage{replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
      InlineKeyboardMarkup [buttonLine]}

type IsTextLine :: TextLine -> Constraint
class IsTextLine a where
  type TextLineData a :: Type
  fromTextLineData :: Proxy a -> TextLineData a -> T.Text

-- induction base
instance KnownSymbol s
      => IsTextLine (TxtLn (Txt s :| '[])) where
  type TextLineData (TxtLn (Txt s :| '[])) = ()
  fromTextLineData _ _ = T.pack $ symbolVal $ Proxy @s

instance IsTextLine (TxtLn (Var a :| '[])) where
  type TextLineData (TxtLn (Var a :| '[])) = T.Text
  fromTextLineData _ var = var

instance IsTextLine (TxtLn (Var' :| '[])) where
  type TextLineData (TxtLn (Var' :| '[])) = T.Text
  fromTextLineData _ = id

instance (KnownSymbol s, IsTextLine (TxtLn (l :| ls)))
      => IsTextLine (TxtLn (Txt s :| l : ls)) where
  type TextLineData (TxtLn (Txt s :| l : ls)) = TextLineData (TxtLn (l :| ls))
  fromTextLineData _ tlData
    =  T.pack (symbolVal $ Proxy @s)
    <> fromTextLineData (Proxy @(TxtLn (l:|ls))) tlData

instance IsTextLine (TxtLn (l :| ls))
      => IsTextLine (TxtLn (Var a :| l : ls)) where
  type TextLineData (TxtLn (Var a :| l : ls)) = (T.Text, TextLineData (TxtLn (l :| ls)))
  fromTextLineData _ (var, tlData)
    =  var
    <> fromTextLineData (Proxy @(TxtLn (l:|ls))) tlData

type IsButtonLine :: ButtonLine -> Constraint
class IsButtonLine a where
  type ButtonLineData a :: Type
  fromButtonLineData :: Proxy a -> ButtonLineData a -> [InlineKeyboardButton]

instance IsButtonLine (BtnLn '[]) where
  type ButtonLineData (BtnLn '[]) = ()
  fromButtonLineData _ _ = []

instance (IsButtonLine (BtnLn bl))
      => IsButtonLine (BtnLn (Btn b : bl)) where
  type ButtonLineData (BtnLn (Btn b : bl)) = (ButtonLineData (BtnLn bl))
  fromButtonLineData _ blData = let
    btn = callbackButton (T.pack "") (T.pack "TODO add callback")
    bl = fromButtonLineData (Proxy @(BtnLn bl)) blData
    in btn : bl
