{-
  This file contains code greatly inspired by Context type from servant-server package
  Original source: https://github.com/haskell-servant/servant/blob/master/servant-server/src/Servant/Server/Internal/Context.hs
 -}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Telegram.Bot.FSAfe.TaggedContext
  ( TaggedContext (..)
  , TaggedContextHasEntry (..)
  , Tagged (..)
  , (.++), type (++), appendTaggedContext
  , let', andLet
  , Rename, rename
  ) where

import Data.Tagged (Tagged (..))

import Data.Kind (Type, Constraint)
import Data.Proxy (Proxy (..))
import GHC.TypeLits (Symbol)
import Unsafe.Coerce (unsafeCoerce)

import Telegram.Bot.FSAfe.FirstClassFamilies (type (++))

type Rename :: Symbol -> Symbol -> [(Symbol, Type)] -> [(Symbol, Type)]
type family Rename x y as where
  Rename x y '[] = '[]
  Rename x y ( '(x, t) : as) = '(y, t) : Rename x y as
  Rename x y ( '(a, t) : as) = '(a, t) : Rename x y as

rename :: Proxy x -> Proxy y -> TaggedContext ctx -> TaggedContext (Rename x y ctx)
rename _ _ = unsafeCoerce
-- for implementation without unsafeCoerce use the following:
--
--   Rename x y ( '(a, t) : as) = '(If (a == x) y a, t) : Rename x y as
-- rename _ _ EmptyTaggedContext = EmptyTaggedContext
-- rename p q (a :. ctx) = retag a :. rename p q ctx

type TaggedContext :: [(Symbol, Type)] -> Type
data TaggedContext as where
  EmptyTaggedContext :: TaggedContext '[]
  (:.) :: Tagged s a -> TaggedContext as -> TaggedContext ('(s, a) : as)

infixr 5 :.

instance Show (TaggedContext '[]) where
  show EmptyTaggedContext = "EmptyTaggedContext"
instance (Show a, Show (TaggedContext as))
      => Show (TaggedContext ('(s, a) ': as)) where
  showsPrec outerPrecedence (a :. as) =
    showParen (outerPrecedence > 5) $
      shows a . showString " :. " . shows as

instance Eq (TaggedContext '[]) where
  _ == _ = True
instance (Eq a, Eq (TaggedContext as)) => Eq (TaggedContext ('(s, a) : as)) where
  x1 :. y1 == x2 :. y2 = x1 == x2 && y1 == y2

let' :: forall s a as. a -> TaggedContext as -> TaggedContext ('(s, a) : as)
let' a ctx = Tagged a :. ctx

andLet :: forall s a. a -> TaggedContext '[ '(s, a)]
andLet a = Tagged a :. EmptyTaggedContext

infixr 5 .++
(.++) :: TaggedContext a -> TaggedContext b -> TaggedContext (a ++ b)
EmptyTaggedContext .++ b = b
(a :. as)   .++ b = a :. (as .++ b)

appendTaggedContext :: TaggedContext a -> TaggedContext b -> TaggedContext (a ++ b)
appendTaggedContext = (.++)

-- | getTaggedContextEntry returns leftmost entry of the key
type TaggedContextHasEntry :: [(Symbol, Type)] -> Symbol -> Type -> Constraint
class TaggedContextHasEntry ctx tag a | ctx tag -> a where
  getTaggedContextEntry :: Proxy tag -> TaggedContext ctx -> a

instance {-# OVERLAPS #-}
         TaggedContextHasEntry ('(tag, a) : as) tag a where
  getTaggedContextEntry _ ((Tagged a) :. _) = a

instance {-# OVERLAPPABLE #-}
         TaggedContextHasEntry as tag a
      => TaggedContextHasEntry (b : as) tag a where
  getTaggedContextEntry proxy (_ :. as) = getTaggedContextEntry proxy as

