{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE DerivingVia #-}

module Telegram.Bot.FSAfe.DSL
  ( ProperMessage(..), Message(..)
  , Proper, Proper'
  , IsMessage(..)
  , TextEntity(..)
  , MessageLine(..)
  , ButtonEntity(..), CallbackBtn, UnitCallbackBtn, CallbackButtons
  , type (:|:), type (:\)
  , RenameTag
  , AsMessage
  , renderMessage
  , IsCallbackData(..)
  , ReadShow(..)
  , IsUnit(..)
  , HasTaggedContext(..)
  , HasFields(..)
  , TypedFields(..)
  , RecordHasTaggedContext(..)
  , RecordFields
  ) where

import qualified Data.Text as T (Text, pack, unlines, unpack)
import Telegram.Bot.API (InlineKeyboardButton, SomeReplyMarkup (..), InlineKeyboardMarkup (..))

import Data.Kind (Type, Constraint)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Proxy (Proxy (..))
import Data.Type.Bool (If)
import GHC.Base (Symbol)
import GHC.TypeLits (KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

import Telegram.Bot.FSAfe.Reply (ReplyMessage(..), toReplyMessage, callbackButton)
import Telegram.Bot.FSAfe.TaggedContext (TaggedContext (..), TaggedContextHasEntry (..), appendTaggedContext, Tagged (..))
import Telegram.Bot.FSAfe.FirstClassFamilies (Exp, Eval, Map, type (++), type (==))
import Text.Read (readMaybe)
import GHC.Generics

import GHC.Records (HasField(..))

data ProperMessage = PMsg (NonEmpty [TextEntity]) [[ButtonEntity]]

data TextEntity
  = Txt Symbol
  | Var Symbol
  | VarShow Symbol

data ButtonEntity
  = CallbackBtn' [TextEntity] Type Symbol
  | UnitCallbackBtn' [TextEntity] Type
  | CallbackButtons' [TextEntity] Type Symbol

type CallbackBtn a = CallbackBtn' (AsTextLine a)
type UnitCallbackBtn a = UnitCallbackBtn' (AsTextLine a)
type CallbackButtons a = CallbackButtons' (AsTextLine a)

data Message = Msg [[TextEntity]] [[ButtonEntity]]

data MessageLine
  = MTL [TextEntity]
  | MBL [ButtonEntity]

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
type instance Eval (RenameButtonTag x y (CallbackBtn' a b c)) =
  CallbackBtn' (Eval (RenameTLTag x y a)) b c

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

instance (Show show, TaggedContextHasEntry ctx a show)
      => IsTextLine (VarShow a : '[]) ctx where
  getTextLine _ = T.pack . show . getTaggedContextEntry (Proxy @a)

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

instance ( IsCallbackData a
         , TaggedContextHasEntry ctx s a
         , HasTaggedContext ctx0 a
         , IsTextLine l (ctx0 ++ ctx)
         )
      => IsButton (CallbackBtn' l a s) ctx where
  getButton _ = do
    a <- getTaggedContextEntry (Proxy @s)
    let ctx0 = getTaggedContext a
    label <- getTextLine (Proxy @l) . appendTaggedContext ctx0
    let callback = toCallbackData a
    return [callbackButton label callback]

instance ( IsCallbackData t
         , IsUnit t
         , IsTextLine tl ctx
         )
      => IsButton (UnitCallbackBtn' tl t) ctx where
  getButton _ = do
    label <- getTextLine (Proxy @tl)
    let callback = toCallbackData $ unitValue @t
    return [callbackButton label callback]

instance ( IsTextLine tl (ctx0 ++ ctx)
         , HasTaggedContext ctx0 a
         , IsCallbackData a
         , TaggedContextHasEntry ctx s [a]
         )
      => IsButton (CallbackButtons' tl a s) ctx where
  getButton _ =
    traverse f =<< getTaggedContextEntry (Proxy @s)
    where
      f :: a -> TaggedContext ctx -> InlineKeyboardButton
      f a = do
        let ctx0 = getTaggedContext a
        label <- getTextLine (Proxy @tl) . appendTaggedContext ctx0
        let callback = toCallbackData a
        return $ callbackButton label callback


class HasTaggedContext ctx a | a -> ctx where
  getTaggedContext :: a -> TaggedContext ctx

class IsCallbackData a where
  toCallbackData :: a -> T.Text
  fromCallbackData :: T.Text -> Maybe a

newtype ReadShow a = ReadShow a
  deriving (Read, Show, Eq)

instance (Read a, Show a) => IsCallbackData (ReadShow a) where
  toCallbackData = T.pack . show
  fromCallbackData = readMaybe . T.unpack

instance IsCallbackData T.Text where
  toCallbackData = id
  fromCallbackData = pure

instance IsCallbackData String where
  toCallbackData = T.pack
  fromCallbackData = pure . T.unpack

class IsUnit u where
  unitValue :: u

  default unitValue :: (Generic u, GIsUnit (Rep u)) => u
  unitValue = to gunitValue

class GIsUnit u where
  gunitValue :: u a

instance GIsUnit U1 where
  gunitValue = U1

class HasFields fields a where
  getFields :: a -> TaggedContext fields

instance {-# OVERLAPPABLE #-}
         HasFields '[] a where
  getFields _ = EmptyTaggedContext

instance {-# OVERLAPS #-}
         (HasField name a ty, HasFields fields a)
      => HasFields ('(name, ty) : fields) a where
  getFields a = Tagged @name (getField @name a) :. getFields a

type TypedFields :: [(Symbol, Type)] -> Type -> Type
newtype TypedFields fields a = TypedFields a

instance HasFields fields a
      => HasTaggedContext fields (TypedFields fields a)  where
  getTaggedContext (TypedFields a) = getFields a

type Fields :: [Symbol] -> Type -> Type
newtype Fields fields a = Fields a

instance {-# OVERLAPS #-}
         (HasFields fields a, fields ~ '[])
      => HasTaggedContext fields (Fields fieldNames a) where
  getTaggedContext (Fields a) = getFields a

class RecordHasTaggedContext a where
  getRecordTaggedContext :: a -> TaggedContext (RecordFields a)

instance {-# OVERLAPPABLE #-}
         (HasFields fields a, fields ~ RecordFields a)
      => HasTaggedContext fields a where
  getTaggedContext = getFields

type RecordFields a = RecordFields' (Rep a)
type RecordFields' :: (Type -> Type) -> [(Symbol, Type)]
type family RecordFields' a where
  RecordFields' (M1 S (MetaSel (Just name) _ _ _) (K1 _ a)) = '[ '(name, a)]
  RecordFields' (M1 D (MetaData dataName _ _ _) x) = RecordFields' x
  RecordFields' (M1 C (MetaCons consName _ _) x)   = RecordFields' x
  RecordFields' (x :*: y) = RecordFields' x ++ RecordFields' y
  RecordFields' _ = '[]

-- type TypeFields fields a = TypeFields' fields (Rep a)
-- type TypeFields' :: [Symbol] -> (Type -> Type) -> [(Symbol, Type)]
-- type family TypeFields' fields a where
--   TypeFields' '[]      _ = '[]
--   TypeFields' (x : xs) a = '(x, TODO) : TypeFields' xs a
