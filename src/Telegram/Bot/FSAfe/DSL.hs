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
  , Proper'
  , IsMessage(..)
  , TextEntity(..)
  , MessageLine(..)
  , ButtonEntity(..), CallbackBtn
  , type (:|:), type (:\)
  , RenameTag
  , AsMessage
  , renderMessage
  ) where

import qualified Data.Text as T (Text, pack, unlines)
import Telegram.Bot.API (InlineKeyboardButton, SomeReplyMarkup (..), InlineKeyboardMarkup (..))

import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

import Telegram.Bot.FSAfe.Reply (ReplyMessage(..), toReplyMessage, callbackButton)
import Telegram.Bot.FSAfe.TaggedContext (TaggedContext, TaggedContextHasEntry (..))
import Telegram.Bot.FSAfe.FirstClassFamilies (Exp, Eval, Map, type (++), type (==))

data ProperMessage = PMsg (NonEmpty [TextEntity]) [[ButtonEntity]]

data TextEntity where
  Txt :: Symbol -> TextEntity
  Var :: Symbol -> TextEntity

data ButtonEntity where
  CallbackBtn' :: [TextEntity] -> [TextEntity] -> ButtonEntity
  CallbackButtons :: Type -> Symbol -> ButtonEntity
  -- UrlBtn :: [TextEntity] -> [TextEntity] -> Button
  -- WebAppBtn, LoginUrlBtn, SwitchInlineQueryBtn,
  -- SwitchInlineQueryCurrentChatBtn, SwitchInlineQueryChosenChatBtn
  -- CallbackGameBtn, PayBtn

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
  AsMessageLine (a :: TextEntity) = MTL '[a]
  AsMessageLine (a :: ButtonEntity) = MBL '[a]
  AsMessageLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to MessageLine")

type JoinMessageLines :: MessageLine -> MessageLine -> MessageLine
type family JoinMessageLines a b where
  JoinMessageLines (MTL a) (MTL b) = MTL (a ++ b)
  JoinMessageLines (MBL a) (MBL b) = MBL (a ++ b)
  JoinMessageLines (MTL a) (MBL b) = JoinMessageLinesError a b
  JoinMessageLines (MBL a) (MTL b) = JoinMessageLinesError a b

type JoinMessageLinesError a b = TypeError
  (Text "Cannot have " :<>: ShowType a :<>: Text " in the same line with " :<>: ShowType b)

infixl 0 :\
type (:\) :: k -> l -> Message
type a :\ b = JoinMessages (AsMessage a) (AsMessage b)

type AsMessage :: k -> Message
type family AsMessage a where
  AsMessage (a :: Message)      = a
  AsMessage (a :: Symbol)       = Msg '[ '[Txt a]] '[]
  AsMessage (a :: TextEntity)   = Msg '[ '[a]]     '[]
  AsMessage (a :: ButtonEntity) = Msg '[]          '[ '[a]]
  AsMessage (MTL a)             = Msg '[a]         '[]
  AsMessage (MBL a)             = Msg '[]          '[a]
  AsMessage a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to Message")

type JoinMessages :: Message -> Message -> Message
type family JoinMessages m1 m2 where
  JoinMessages (Msg tls1 '[]) (Msg tls2 bls) = Msg (tls1 ++ tls2) bls
  JoinMessages (Msg tls bls1) (Msg '[] bls2) = Msg tls (bls1 ++ bls2)
  JoinMessages (Msg _ (_:_))  (Msg (_:_) _)  = TypeError (Text "Cannot have text below buttons")

type CallbackBtn :: k -> l -> ButtonEntity
type family CallbackBtn a b where
  CallbackBtn a b = CallbackBtn' (AsTextLine a) (AsTextLine b)

type AsTextLine :: k -> [TextEntity]
type family AsTextLine a where
  AsTextLine (MTL a) = a
  AsTextLine (a :: [TextEntity]) = a
  AsTextLine (a :: TextEntity)   = '[a]
  AsTextLine (a :: Symbol)       = '[Txt a]
  AsTextLine a = TypeError (Text "Cannot convert " :<>: ShowType a :<>: Text " to TextLine")

type Proper :: k -> ProperMessage
type Proper x = Proper' (AsMessage x)

type Proper' :: Message -> ProperMessage
type family Proper' msg where
  Proper' (Msg (tl:tls) bls) = PMsg
    (Eval (ProperTL tl) :| Eval (Map ProperTL tls))
    (Eval (Map ProperBL bls))
  Proper' (Msg '[] _) = TypeError (Text "Cannot have a message without text")

data ProperTL :: [TextEntity] -> Exp [TextEntity]
type instance Eval (ProperTL '[]) = TypeError (Text "Cannot have empty text line")
type instance Eval (ProperTL (tl:tls)) = tl:tls

data ProperBL :: [ButtonEntity] -> Exp [ButtonEntity]
type instance Eval (ProperBL a) = a

renderMessage :: forall k {msg :: k} ctx. IsMessage (Proper msg) ctx
              => Proxy msg -> TaggedContext ctx -> ReplyMessage
renderMessage _ = getMessage (Proxy @(Proper msg))

type RenameTag :: Symbol -> Symbol -> Message -> Message
type family RenameTag x y msg where
  RenameTag x x msg = msg
  RenameTag x y (Msg tls bls) = Msg
    (Eval (Map (RenameTLTag x y) tls))
    (Eval (Map (RenameBLTag x y) bls))

type RenameTLTag x y = Map (RenameTextEntityTag x y)
data RenameTextEntityTag :: Symbol -> Symbol -> TextEntity -> Exp TextEntity
type instance Eval (RenameTextEntityTag x y (Var a)) = If (x == a) (Var y) (Var a)
type instance Eval (RenameTextEntityTag _ _ (Txt a)) = Txt a

type RenameBLTag x y = Map (RenameButtonTag x y)
data RenameButtonTag :: Symbol -> Symbol -> ButtonEntity -> Exp ButtonEntity
type instance Eval (RenameButtonTag x y (CallbackBtn' a b)) =
  CallbackBtn' (Eval (RenameTLTag x y a)) (Eval (RenameTLTag x y b))

type IsMessage :: ProperMessage -> [(Symbol, Type)] -> Constraint
class IsMessage a ctx where
  getMessage :: Proxy a -> TaggedContext ctx -> ReplyMessage

instance IsTextLine tl ctx
      => IsMessage (PMsg (tl :| '[]) '[]) ctx where
  getMessage _ ctx = let
    messageMarkup = SomeInlineKeyboardMarkup $ InlineKeyboardMarkup []
    textMessage = toReplyMessage $ getTextLine (Proxy @tl) ctx
    in textMessage{replyMessageReplyMarkup = Just messageMarkup}

instance (IsTextLine tl ctx, IsMessage (PMsg (tl1 :| tls) '[]) ctx)
      => IsMessage (PMsg (tl :| tl1 : tls) '[]) ctx where
  getMessage _ = do
    replyMessage <- getMessage (Proxy @(PMsg (tl1 :| tls) '[]))
    textLine <- getTextLine (Proxy @tl)
    let newText = T.unlines
            [ textLine
            , replyMessageText replyMessage ]
    return $ replyMessage{replyMessageText = newText}

instance (IsButtonLine bl ctx, IsMessage (PMsg tls bls) ctx)
      => IsMessage (PMsg tls (bl : bls)) ctx where
  getMessage _ = do
    replyMessage <- getMessage (Proxy @(PMsg tls bls))
    buttonLine <- getButtonLine (Proxy @bl)
    let initialButtons = concatMap f $ replyMessageReplyMarkup replyMessage
    return $ replyMessage{replyMessageReplyMarkup = Just $ SomeInlineKeyboardMarkup $
      InlineKeyboardMarkup (buttonLine : initialButtons)}
    where
      f (SomeInlineKeyboardMarkup (InlineKeyboardMarkup x)) = x
      f _ = []

type IsTextLine :: [TextEntity] -> [(Symbol, Type)] -> Constraint
class IsTextLine a ctx where
  getTextLine :: Proxy a -> TaggedContext ctx -> T.Text

instance KnownSymbol s
      => IsTextLine (Txt s : '[]) ctx where
  getTextLine _ _ = T.pack $ symbolVal $ Proxy @s

instance TaggedContextHasEntry ctx a T.Text
      => IsTextLine (Var a : '[]) ctx where
  getTextLine _ = getTaggedContextEntry (Proxy @a)

instance (KnownSymbol s, IsTextLine (l : ls) ctx)
      => IsTextLine (Txt s : l : ls) ctx where
  getTextLine _ tlData
    =  T.pack (symbolVal $ Proxy @s)
    <> getTextLine (Proxy @(l:ls)) tlData

instance (IsTextLine (l : ls) ctx, TaggedContextHasEntry ctx a T.Text)
      => IsTextLine (Var a : l : ls) ctx where
  getTextLine _ = do
    var <- getTaggedContextEntry (Proxy @a)
    textLine <- getTextLine (Proxy @(l:ls))
    return $ var <> textLine

type IsButtonLine :: [ButtonEntity] -> [(Symbol, Type)] -> Constraint
class IsButtonLine a ctx where
  getButtonLine :: Proxy a -> TaggedContext ctx -> [InlineKeyboardButton]

instance IsButtonLine '[] ctx where
  getButtonLine _ _ = []

instance (IsButton b ctx, IsButtonLine bl ctx)
      => IsButtonLine (b : bl) ctx where
  getButtonLine _ = do
    btn <- getButton (Proxy @b)
    bl <- getButtonLine (Proxy @bl)
    return $ btn ++ bl

type IsButton :: ButtonEntity -> [(Symbol, Type)] -> Constraint
class IsButton a ctx where
  getButton :: Proxy a -> TaggedContext ctx -> [InlineKeyboardButton]

instance (IsTextLine l ctx, IsTextLine c ctx)
      => IsButton (CallbackBtn' l c) ctx where
  getButton _ = do
    label <- getTextLine (Proxy @l)
    callback <- getTextLine (Proxy @c)
    return [callbackButton label callback]

instance (TaggedContextHasEntry ctx s (a -> InlineKeyboardButton, [a]))
      => IsButton (CallbackButtons a s) ctx where
  getButton _ = do
    (f, xs) <- getTaggedContextEntry (Proxy @s)
    return $ f <$> xs

