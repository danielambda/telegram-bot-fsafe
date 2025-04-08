{-
  This file contains code greatly inspired by Context type from servant-server package
  Original sourse: https://github.com/haskell-servant/servant/blob/master/servant-server/src/Servant/Server/Internal/Context.hs
 -}

{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.FSAfe.MessageContext (MessageContext(..), (.++), getMessageContextEntry) where

import Data.Kind (Type, Constraint)
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))
import Data.Proxy (Proxy (..))

type MessageContext :: [(Symbol, Type)] -> Type
data MessageContext as where
  EmptyMsgCtx :: MessageContext '[]
  (:.) :: (Proxy s, a) -> MessageContext as -> MessageContext ('(s, a) ': as)

infixr 5 :.

instance Show (MessageContext '[]) where
  show EmptyMsgCtx = "EmptyMsgCtx"
instance (KnownSymbol s, Show a, Show (MessageContext as))
      => Show (MessageContext ('(s, a) ': as)) where
  showsPrec outerPrecedence (a :. as) =
    showParen (outerPrecedence > 5) $
      showString ("\"" <> symbolVal (Proxy @s) <> "\" ") . shows a . showString " :. " . shows as

instance Eq (MessageContext '[]) where
  _ == _ = True
instance (Eq a, Eq (MessageContext as)) => Eq (MessageContext ('(s, a) ': as)) where
  x1 :. y1 == x2 :. y2 = x1 == x2 && y1 == y2

type (++) :: [k] -> [k] -> [k]
type family as ++ bs where
  '[] ++ a = a
  (a ': as) ++ b = a ': (as ++ b)

(.++) :: MessageContext l1 -> MessageContext l2 -> MessageContext (l1 ++ l2)
EmptyMsgCtx .++ a = a
(a :. as) .++ b = a :. (as .++ b)

type Lookup :: a -> [(a, b)] -> Maybe b
type family Lookup key list where
  Lookup key '[] = 'Nothing
  Lookup key ('(key, a) : _) = Just a
  Lookup key (_ : as) = Lookup key as

type LookupOrError :: a -> [(a, b)] -> ErrorMessage -> b
type family LookupOrError key list errorMessage where
  LookupOrError key list e = FromMaybe (TypeError e) (Lookup key list)

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe a maybea where
  FromMaybe a Nothing  = a
  FromMaybe _ (Just a) = a

type MessageContextHasEntry :: [(Symbol, Type)] -> Symbol -> Constraint
class MessageContextHasEntry ctx key where
  getMessageContextEntry
    :: Proxy key
    -> MessageContext ctx
    -> LookupOrError key ctx
      (    Text "MessageContext " :<>: ShowType ctx
      :<>: Text " does not contain key " :<>: Text "\"" :<>: Text key :<>: Text "\""
      )

instance {-# OVERLAPS #-}
         MessageContextHasEntry ('(key, a) : as) key where
  getMessageContextEntry _ ((_, a) :. _) = a

-- I have no idea how this combination of equality constraints work here, by the way
instance {-# OVERLAPPABLE #-}
         ( MessageContextHasEntry as key
         , Lookup key as ~ Lookup key ('(notKey, a) : as)
         , Lookup key as ~ Just something
         ) => MessageContextHasEntry ('(notKey, a) : as) key where
  getMessageContextEntry proxy (_ :. a) = getMessageContextEntry proxy a
