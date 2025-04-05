{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Telegram.Bot.FSAfe.UndecidableInstances where
import Data.Kind (Type)
import GHC.TypeLits (Symbol, AppendSymbol, SSymbol, TypeError, ErrorMessage(..))
import Data.List.NonEmpty (NonEmpty(..))

newtype TextLine = TxtLn (NonEmpty Type)

newtype ButtonLine = BtnLn [Type]

newtype Keyboard = Kbrd [ButtonLine]

-- data Message = Message (NonEmpty TextLine) (Maybe Keyboard)
data Message = Msg (NonEmpty TextLine) (Maybe Keyboard)

type Var :: k -> Type
data Var name

type Var' :: Type
data Var'

type Btn :: TextLine -> [Type] -> Type
data Btn text callback

-- TODO think about removing vars alltogether, replace Var' with ()
-- and treat anything else that is not a symbol as a key for var
infixr 9 :|:
type (:|:) :: k -> l -> TextLine
type family a :|: b where
  (a :: Symbol) :|: (b :: Symbol) = TxtLn (SSymbol (a `AppendSymbol` b) :|'[])
  (a :: Symbol) :|: Var b         = TxtLn (SSymbol a :|'[Var b])
  (a :: Symbol) :|: Var'          = TxtLn (SSymbol a :|'[Var'])
  (a :: Symbol) :|: TxtLn (b:|bs) = TxtLn (SSymbol a :| b : bs)
  Var a         :|: (b :: Symbol) = TxtLn (Var a :|'[SSymbol b])
  Var a         :|: Var b         = TxtLn (Var a :|'[Var b])
  Var a         :|: Var'          = TxtLn (Var a :|'[Var'])
  Var a         :|: TxtLn (b:|bs) = TxtLn (Var a :| b : bs)
  Var'          :|: (b :: Symbol) = TxtLn (Var' :|'[SSymbol b])
  Var'          :|: Var b         = TxtLn (Var' :|'[Var b])
  Var'          :|: Var'          = TxtLn (Var' :|'[Var'])
  Var'          :|: TxtLn (b:|bs) = TxtLn (Var' :| b : bs)
  TxtLn a       :|: (b :: Symbol) = TxtLn (a ++ SSymbol b :|'[])
  TxtLn a       :|: Var b         = TxtLn (a ++ Var b :|'[])
  TxtLn a       :|: Var'          = TxtLn (a ++ Var' :|'[])
  TxtLn a       :|: TxtLn b       = TxtLn (a ++ b)
  _ :|: _ = TypeError (Text "TODO add text message")

infixr 9 :#:
type (:#:) :: k -> l -> ButtonLine
type family a :#: b where
  Btn a a1 :#: Btn b b1 = BtnLn '[Btn a a1, Btn b b1]
  BtnLn a  :#: Btn b b1 = BtnLn (a ++ '[Btn b b1])
  Btn a a1 :#: BtnLn b  = BtnLn (Btn a a1 : b)

infixl 0 \|
type (\|) :: k -> l -> Message
type family a \| b where
  (a :: Symbol)  \| (b :: Symbol)     = Msg (TxtLn (SSymbol (a `AppendSymbol` "\n" `AppendSymbol` b) :|'[]) :|'[]) Nothing
  (a :: Symbol)  \| TxtLn b           = Msg (TxtLn (SSymbol a :|'[]) :| '[TxtLn b]) Nothing
  (a :: Symbol)  \| BtnLn b           = Msg (TxtLn (SSymbol a :|'[]) :| '[]) (Just (Kbrd '[BtnLn b]))
  (a :: Symbol)  \| Msg (l :| ls) kb  = Msg (TxtLn (SSymbol a :|'[]) :| l : ls) kb
  TxtLn a        \| (b :: Symbol)     = Msg (TxtLn a :| '[TxtLn (SSymbol b :|'[])]) Nothing
  TxtLn a        \| TxtLn b           = Msg (TxtLn a :| '[TxtLn b]) Nothing
  TxtLn a        \| BtnLn b           = Msg (TxtLn a :| '[]) (Just (Kbrd '[BtnLn b]))
  TxtLn a        \| Msg (l :| ls) kb  = Msg (TxtLn a :| l : ls) kb
  Msg ls kb      \| (b :: Symbol)     = Msg (ls ++ TxtLn (SSymbol b :|'[]) :| '[]) kb
  Msg ls kb      \| TxtLn b           = Msg (ls ++ TxtLn b :| '[]) kb
  Msg ls Nothing \| BtnLn b           = Msg ls (Just (Kbrd '[BtnLn b]))
  Msg ls Nothing \| Msg ls1 kb        = Msg (ls ++ ls1) kb
  Msg ls (Just (Kbrd kb)) \| BtnLn b  = Msg ls (Just (Kbrd (BtnLn b : kb)))
  Msg ls (Just (Kbrd kb)) \| Btn b bs = Msg ls (Just (Kbrd (BtnLn '[Btn b bs] : kb)))
  Msg _  (Just _) \| _ = TypeError (Text "Cannot add anything but a button(s) to a message with buttons")

infixl 1 ++
type (++) :: f k -> f k -> f k
type family as ++ bs where
  '[]      ++ bs = bs
  (a : as) ++ bs = a : (as ++ bs)
  (a :| '[]) ++ (b :| bs) = a :| b : bs
  (a :| a1 : as) ++ (b :| bs) = a :| a1 : b : (as ++ bs)
