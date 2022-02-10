module Test.Snapshots.DataDecls where

import Data.Tuple (Tuple)

data NoCtors

data OneCtor = OneCtor

data TwoCtors
  = Ctor1
  | Ctor2

newtype NType = NType Int

type TypeAlias = { a :: Int }

newtype NTypeAlias = NTypeAlias TypeAlias

data DTyVars a b (c :: Type) = DTyVars a b c
newtype NTyVars a b (c :: Type) = NTyVars (DTyVars a b c)
type TTyVars a b (c :: Type) = DTyVars a b c

data Complex :: Type -> (Type -> Type) -> Type -> Type
data Complex a b c
  = C1
  | C2 a
  | C3 (b c)
  | C4 (Tuple a (b c))
  | C5 { foo :: a }
  | C6 (Complex a b c)
  | C7 (forall n. n -> a)
