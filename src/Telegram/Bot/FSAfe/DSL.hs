{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Telegram.Bot.FSAfe.DSL
  ( ProperMessage(..), Message(..), Proper
  , TextLine(..), TextEntity(..)
  , ButtonLine(..), Button(..)
  , MessageLine(..)
  , renderMessage
  , type (:|:), type (:\)
  ) where

import qualified Data.Text as T (Text, pack, unlines)
import Telegram.Bot.API (InlineKeyboardButton, SomeReplyMarkup (..), InlineKeyboardMarkup (..), labeledInlineKeyboardButton)

import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy (..))
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

import Telegram.Bot.FSAfe.Reply (ReplyMessage(..), toReplyMessage)
import Telegram.Bot.FSAfe.TaggedContext (TaggedContext (..), TaggedContextHasEntry (..))
import Telegram.Bot.FSAfe.FirstClassFamilies (Exp, Eval, Map, type (++))

data ProperMessage = PMsg (NonEmpty TextLine) [ButtonLine]

newtype TextLine = TxtLn (NonEmpty TextEntity)

data TextEntity where
  Txt :: Symbol -> TextEntity
  Var :: Symbol -> TextEntity

newtype ButtonLine = BtnLn [Button]

data Button where
  LabelBtn :: TextLine -> Button

data Message = Msg [[TextEntity]] [[Button]]

data MessageLine where
  MTL :: [TextEntity] -> MessageLine
  MBL :: [Button] -> MessageLine

infixr 9 :|:
type (:|:) :: k -> l -> MessageLine
type a :|: b = JoinMessageLines (AsMessageLine a) (AsMessageLine b)

type AsMessageLine :: k -> MessageLine
type family AsMessageLine a where
  AsMessageLine (MTL a) = MTL a
  AsMessageLine (MBL a) = MBL a
  AsMessageLine (a :: Symbol) = MTL '[Txt a]
  AsMessageLine (Var a) = MTL '[Var a]
  AsMessageLine (LabelBtn a) = MBL '[LabelBtn a]
  AsMessageLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to MessageLine")

type JoinMessageLines :: MessageLine -> MessageLine -> MessageLine
type family JoinMessageLines ml1 ml2 where
  JoinMessageLines (MTL tl1) (MTL tl2) = MTL (tl1 ++ tl2)
  JoinMessageLines (MBL bl1) (MBL bl2) = MBL (bl1 ++ bl2)
  JoinMessageLines (MTL tl)  (MBL bl) = JoinMessageLinesError tl bl
  JoinMessageLines (MBL bl)  (MTL tl) = JoinMessageLinesError bl tl

type JoinMessageLinesError a b = TypeError
  (Text "Cannot have " :<>: ShowType a :<>: Text " in the same line with " :<>: ShowType b)

infixl 0 :\
type (:\) :: k -> l -> Message
type a :\ b = JoinMessages (AsMessage a) (AsMessage b)

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
  AsMessage (LabelBtn a)  = Msg '[]          '[ '[LabelBtn a]]
  AsMessage (MTL a)       = Msg '[a]         '[]
  AsMessage (MBL a)       = Msg '[]          '[a]
  AsMessage a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to Message")

type Proper :: Message -> ProperMessage
type family Proper msg where
  Proper (Msg (tl:tls) bls) = PMsg
    (Eval (ProperTL tl) :| Eval (Map ProperTL tls))
    (Eval (Map ProperBL bls))
  Proper (Msg '[] _) = TypeError (Text "Cannot have a message without text")

data ProperTL :: [TextEntity] -> Exp TextLine
type instance Eval (ProperTL '[]) = TypeError (Text "Cannot have empty text line")
type instance Eval (ProperTL (tl:tls)) = TxtLn (tl:|tls)

data ProperBL :: [Button] -> Exp ButtonLine
type instance Eval (ProperBL a) = BtnLn a

renderMessage
  :: forall msg ctx. (IsMessage (Proper msg) ctx)
  => Proxy msg -> TaggedContext ctx -> ReplyMessage
renderMessage _ = fromMessageData (Proxy @(Proper msg))

type IsMessage :: ProperMessage -> [(Symbol, Type)] -> Constraint
class All (TaggedContextHasEntry ctx) (MessageData a)
   => IsMessage a ctx where
  type MessageData a :: [(Symbol, Type)]
  fromMessageData :: Proxy a -> TaggedContext ctx -> ReplyMessage

instance IsTextLine tl ctx
      => IsMessage (PMsg (tl :| '[]) '[]) ctx where
  type MessageData (PMsg (tl :| '[]) '[]) = TextLineData tl
  fromMessageData _ ctx = let
    messageMarkup = SomeInlineKeyboardMarkup $ InlineKeyboardMarkup []
    textMessage = toReplyMessage $ fromTextLineData (Proxy @tl) ctx
    in textMessage{replyMessageReplyMarkup = Just messageMarkup}

instance ( IsTextLine (TxtLn tl) ctx
         , IsMessage (PMsg (tl1 :| tls) '[]) ctx
         , All (TaggedContextHasEntry ctx) (MessageData (PMsg (TxtLn tl :| tl1 : tls) '[]))
         )
      => IsMessage (PMsg (TxtLn tl :| tl1 : tls) '[]) ctx where
  type MessageData (PMsg (TxtLn tl :| tl1 : tls) '[]) =
    TextLineData (TxtLn tl) ++ MessageData (PMsg (tl1 :| tls) '[])
  fromMessageData _ ctx = let
    replyMessage = fromMessageData (Proxy @(PMsg (tl1 :| tls) '[])) ctx
    newText = T.unlines
            [ fromTextLineData (Proxy @(TxtLn tl)) ctx
            , replyMessageText replyMessage ]
    in replyMessage{replyMessageText = newText}

instance ( IsButtonLine (BtnLn bl) ctx
         , IsMessage (PMsg tls bls) ctx
         , All (TaggedContextHasEntry ctx) (MessageData (PMsg tls (BtnLn bl : bls)))
         )
      => IsMessage (PMsg tls (BtnLn bl : bls)) ctx where
  type MessageData (PMsg tls (BtnLn bl : bls)) =
    ButtonLineData (BtnLn bl) ++ MessageData (PMsg tls bls)
  fromMessageData _ ctx = let
    replyMessage = fromMessageData (Proxy @(PMsg tls bls)) ctx
    buttonLine = fromButtonLineData (Proxy @(BtnLn bl)) ctx
    in replyMessage{replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
      InlineKeyboardMarkup [buttonLine]}

type IsTextLine :: TextLine -> [(Symbol, Type)] -> Constraint
class All (TaggedContextHasEntry ctx) (TextLineData a)
   => IsTextLine a ctx where
  type TextLineData a :: [(Symbol, Type)]
  fromTextLineData :: Proxy a -> TaggedContext ctx -> T.Text

type All :: (a -> b -> Constraint) -> [(a, b)] -> Constraint
type family All f x where
  All _ '[] = ()
  All f ('(a,b):abs) = (f a b, All f abs)

instance KnownSymbol s
      => IsTextLine (TxtLn (Txt s :| '[])) ctx where
  type TextLineData (TxtLn (Txt s :| '[])) = '[]
  fromTextLineData _ _ = T.pack $ symbolVal $ Proxy @s

instance TaggedContextHasEntry ctx a T.Text
      => IsTextLine (TxtLn (Var a :| '[])) ctx where
  type TextLineData (TxtLn (Var a :| '[])) = '[ '(a, T.Text)]
  fromTextLineData _ = getTaggedContextEntry (Proxy @a)

instance (KnownSymbol s, IsTextLine (TxtLn (l :| ls)) ctx)
      => IsTextLine (TxtLn (Txt s :| l : ls)) ctx where
  type TextLineData (TxtLn (Txt s :| l : ls)) = TextLineData (TxtLn (l :| ls))
  fromTextLineData _ tlData
    =  T.pack (symbolVal $ Proxy @s)
    <> fromTextLineData (Proxy @(TxtLn (l:|ls))) tlData

instance (IsTextLine (TxtLn (l :| ls)) ctx, TaggedContextHasEntry ctx a T.Text)
      => IsTextLine (TxtLn (Var a :| l : ls)) ctx where
  type TextLineData (TxtLn (Var a :| l : ls)) = '(a, T.Text) : TextLineData (TxtLn (l :| ls))
  fromTextLineData _ ctx = let
    var = getTaggedContextEntry (Proxy @a) ctx
    in var <> fromTextLineData (Proxy @(TxtLn (l:|ls))) ctx

type IsButtonLine :: ButtonLine -> [(Symbol, Type)] -> Constraint
class All (TaggedContextHasEntry ctx) (ButtonLineData a) => IsButtonLine a ctx where
  type ButtonLineData a :: [(Symbol, Type)]
  fromButtonLineData :: Proxy a -> TaggedContext ctx -> [InlineKeyboardButton]

instance IsButtonLine (BtnLn '[]) ctx where
  type ButtonLineData (BtnLn '[]) = '[]
  fromButtonLineData _ _ = []

instance ( IsButtonLine (BtnLn bl) ctx
         , IsTextLine tl ctx
         , All (TaggedContextHasEntry ctx) (ButtonLineData (BtnLn (LabelBtn tl : bl)))
         )
      => IsButtonLine (BtnLn (LabelBtn tl : bl)) ctx where
  type ButtonLineData (BtnLn (LabelBtn tl : bl)) =
    TextLineData tl ++ ButtonLineData (BtnLn bl)
  fromButtonLineData _ ctx = let
    label = fromTextLineData (Proxy @tl) ctx
    btn = labeledInlineKeyboardButton label
    bl = fromButtonLineData (Proxy @(BtnLn bl)) ctx
    in btn : bl
