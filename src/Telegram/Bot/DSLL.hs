{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.DSLL where

import Data.Foldable (toList)
import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import GHC.TypeLits (KnownNat, natVal, Symbol, Nat, KnownSymbol, symbolVal)
import Text.Read (readMaybe)

import qualified Data.Text as T (Text, pack, unpack)
import qualified Telegram.Bot.API as TG

import Telegram.Bot.DSL.Classes.HasTaggedContext (HasTaggedContext (..))
import Telegram.Bot.DSL.Components.TextLine (IsTextLine(..), Var, Txt)
import Telegram.Bot.DSL.TaggedContext
  (TaggedContext (..), TaggedContextHasEntry(..), type (++), (.++), let', andLet)

class HasButton a state ctx where
  getButton :: Proxy a -> state -> TaggedContext ctx -> TG.InlineKeyboardButton

class HasButtons a state ctx where
  getButtons :: Proxy a -> state -> TaggedContext ctx -> [TG.InlineKeyboardButton]

class HasKeyboard a state ctx where
  getKeyboard :: Proxy a -> state -> TaggedContext ctx -> [[TG.InlineKeyboardButton]]

instance {-# OVERLAPPABLE #-} HasButton a state ctx
      => HasButtons a state ctx where
  getButtons proxy state ctx = [getButton proxy state ctx]

instance HasButtons '[] state ctx where
  getButtons _ _ _ = []

instance (HasButton a state ctx, HasButtons as state ctx) => HasButtons (a ': as) state ctx where
  getButtons _ = liftA2 (:) <$> getButton (Proxy @a) <*> getButtons (Proxy @as)

instance {-# OVERLAPPABLE #-} HasButtons a state ctx
      => HasKeyboard a state ctx where
  getKeyboard proxy state ctx = [getButtons proxy state ctx]

callbackButton :: T.Text -> T.Text -> TG.InlineKeyboardButton
callbackButton label callback = (TG.labeledInlineKeyboardButton label)
  {TG.inlineKeyboardButtonCallbackData = Just callback}

class IsCallbackData' state callback where
  toCallbackData' :: state -> callback -> T.Text
  fromCallbackData' :: state -> T.Text -> Maybe callback

instance {-# OVERLAPPABLE #-} IsCallbackData callback => IsCallbackData' anyState callback where
  toCallbackData' _ = toCallbackData
  fromCallbackData' _ = fromCallbackData

class IsCallbackData callback where
  toCallbackData :: callback -> T.Text
  fromCallbackData :: T.Text -> Maybe callback

type Button :: k -> Type -> Symbol -> Type
data Button textLine callback key
instance ( TaggedContextHasEntry ctx0 key callback
         , HasTaggedContext ctx1 callback
         , ctx ~ ctx1 ++ ctx0
         , IsCallbackData' state callback
         , IsTextLine textLine ctx
         )
      => HasButton (Button textLine callback key) state ctx0 where
  getButton _ state ctx0 =
    let callback = getTaggedContextEntry (Proxy @key) ctx0
        ctx1 = getTaggedContext callback
        ctx = ctx1 .++ ctx0
        textLine = getTextLine (Proxy @textLine) ctx
        callbackData = toCallbackData' state callback
    in callbackButton textLine callbackData

type ForEach :: Symbol -> k -> Type
data ForEach items buttons
instance ( Foldable f
         , TaggedContextHasEntry ctx0 items (f item)
         , HasTaggedContext itemCtx item
         , ctx ~ itemCtx ++ ctx0
         , HasButtons buttons state ctx
         )
      => HasButtons (ForEach items buttons) state ctx0 where
  getButtons _ state ctx0 = do
    item <- toList $ getTaggedContextEntry (Proxy @items) ctx0
    let itemCtx = getTaggedContext item
    let ctx = itemCtx .++ ctx0
    getButtons (Proxy @buttons) state ctx

data Row buttons
instance HasButtons a state ctx => HasKeyboard (Row a) state ctx where
  getKeyboard _ state ctx = [getButtons (Proxy @a) state ctx]

data Col buttons
instance HasButtons a state ctx => HasKeyboard (Col a) state ctx where
  getKeyboard _ state ctx = map (:[]) $ getButtons (Proxy @a) state ctx

type Grid :: Nat -> k -> Type
data Grid width button
instance (HasButtons a state ctx, KnownNat w) => HasKeyboard (Grid w a) state ctx where
  getKeyboard _ state ctx = chunksOfW $ getButtons (Proxy @a) state ctx
    where
      w = fromInteger $ natVal $ Proxy @w
      chunksOfW [] = []
      chunksOfW ys = chunk : chunksOfW rest
        where (chunk, rest) = splitAt w ys

data TextE where
  C :: Symbol -> TextE
  Var :: Symbol -> TextE
  (:|:) :: a -> b -> TextE

type F :: Symbol -> Type
data F text

type HasTextE :: [(Symbol, Type)] -> TextE -> Constraint
class HasTextE ctx te where
  textE :: Proxy te -> TaggedContext ctx -> T.Text

instance KnownSymbol text => HasTextE ctx (C text) where
  textE _ _ = T.pack $ symbolVal $ Proxy @text

instance (HasTextE ctx a, HasTextE ctx b) => HasTextE ctx (a :|: b) where
  textE _ = (<>) <$> textE (Proxy @a) <*> textE (Proxy @b)

----------------------- EXAMPLE -----------------------

newtype Cancel = Cancel { aboba :: T.Text } deriving (Generic, Show)
data Aboba = Aboba { btn1 :: (), btn2 :: Cancel } deriving Generic
type Btns = Grid 2 ("Keys" `ForEach`
   '[ Button '[Var "aboba"] Cancel "btn0"
    , Button '[Txt "text"] () "btn1"
    ]
  )

instance IsCallbackData' T.Text Cancel where
  toCallbackData' t _ = t
  fromCallbackData' t _ = Just $ Cancel t

instance IsCallbackData () where
  toCallbackData = T.pack . show
  fromCallbackData = readMaybe . T.unpack

-- >>> map TG.inlineKeyboardButtonText <$> btns
-- [["abbba","text"],["abbba","text"],["abbba","text"]]
btns :: [[TG.InlineKeyboardButton]]
btns = getKeyboard (Proxy @Btns) (T.pack "CALLBACK")
  $ let' @"btn0" (Cancel (T.pack "abbba"))
  $ andLet @"Keys"
    [ Aboba () $ Cancel (T.pack "a")
    , Aboba () $ Cancel (T.pack "b")
    , Aboba () $ Cancel (T.pack "ab")
    ]
