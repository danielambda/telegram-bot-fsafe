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

module Telegram.Bot.FSAfe.MessageContext
  ( MessageContext(..)
  , (.++)
  , MessageContextHasEntry(..)
  ) where

import Data.Tagged (Tagged (..))

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal, TypeError, ErrorMessage(..))

type MessageContext :: [(Symbol, Type)] -> Type
data MessageContext as where
  EmptyMsgCtx :: MessageContext '[]
  (:.) :: Tagged s a -> MessageContext as -> MessageContext ('(s, a) ': as)

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
type family Lookup tag list where
  Lookup tag '[] = 'Nothing
  Lookup tag ('(tag, a) : _) = Just a
  Lookup tag (_ : as) = Lookup tag as

type LookupOrError :: a -> [(a, b)] -> ErrorMessage -> b
type family LookupOrError tag list errorMessage where
  LookupOrError tag list e = FromMaybe (TypeError e) (Lookup tag list)

type FromMaybe :: a -> Maybe a -> a
type family FromMaybe a maybea where
  FromMaybe a Nothing  = a
  FromMaybe _ (Just a) = a

type MessageContextHasEntry :: [(Symbol, Type)] -> Symbol -> Constraint
class MessageContextHasEntry ctx tag where
  getMessageContextEntry
    :: Proxy tag
    -> MessageContext ctx
    -> LookupOrError tag ctx
      (    Text "MessageContext " :<>: ShowType ctx
      :<>: Text " does not contain tag " :<>: Text "\"" :<>: Text tag :<>: Text "\""
      )

instance {-# OVERLAPS #-}
         MessageContextHasEntry ('(tag, a) : as) tag where
  getMessageContextEntry _ ((Tagged a) :. _) = a

-- I have no idea how this combination of equality constraints work here, by the way
instance {-# OVERLAPPABLE #-}
         ( MessageContextHasEntry as tag
         , Lookup tag as ~ Lookup tag ('(nottag, a) : as)
         , Lookup tag as ~ Just something
         ) => MessageContextHasEntry ('(nottag, a) : as) tag where
  getMessageContextEntry proxy (_ :. a) = getMessageContextEntry proxy a
