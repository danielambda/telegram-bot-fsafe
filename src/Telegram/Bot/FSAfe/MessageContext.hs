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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

module Telegram.Bot.FSAfe.MessageContext
  ( MessageContext (..)
  , MessageContextHasEntry (..)
  , Tagged (..)
  ) where

import Data.Tagged (Tagged (..))

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy)
import GHC.TypeLits (Symbol)

type family NotElem a as where
  NotElem _ '[] = 'True
  NotElem a (a:as) = 'False
  NotElem a (b:bs) = NotElem a bs

type Exp a = a -> Type

type Eval :: Exp a -> a
type family Eval e

data Map :: (a -> Exp b) -> [a] -> Exp [b]
type instance Eval (Map _ '[]) = '[]
type instance Eval (Map f (x:xs)) = Eval (f x) : Eval (Map f xs)

data Fst :: (a, b) -> Exp a
type instance Eval (Fst '(a, _)) = a

type MessageContext :: [(Symbol, Type)] -> Type
data MessageContext as where
  EmptyMsgCtx :: MessageContext '[]
  (:.) :: Tagged s a -> MessageContext as -> MessageContext ('(s, a) ': as)

infixr 5 :.

instance Show (MessageContext '[]) where
  show EmptyMsgCtx = "EmptyMsgCtx"
instance (Show a, Show (MessageContext as))
      => Show (MessageContext ('(s, a) ': as)) where
  showsPrec outerPrecedence (a :. as) =
    showParen (outerPrecedence > 5) $
      shows a . showString " :. " . shows as

instance Eq (MessageContext '[]) where
  _ == _ = True
instance (Eq a, Eq (MessageContext as)) => Eq (MessageContext ('(s, a) ': as)) where
  x1 :. y1 == x2 :. y2 = x1 == x2 && y1 == y2

type MessageContextHasEntry :: [(Symbol, Type)] -> Symbol -> Type -> Constraint
class MessageContextHasEntry ctx tag a | ctx tag -> a where
  getMessageContextEntry :: Proxy tag -> MessageContext ctx -> a

instance {-# OVERLAPS #-}
         MessageContextHasEntry ('(tag, a) : as) tag a where
  getMessageContextEntry _ ((Tagged a) :. _) = a

-- I have no idea how this combination of equality constraints work here, by the way
instance {-# OVERLAPPABLE #-}
         MessageContextHasEntry as tag a
      => MessageContextHasEntry (b : as) tag a where
  getMessageContextEntry proxy (_ :. as) = getMessageContextEntry proxy as

