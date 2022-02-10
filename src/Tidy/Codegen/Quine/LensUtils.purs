module Tidy.Codegen.Quine.LensUtils where

import Prelude

import Data.Array as Array
import Data.Lens (Fold', Lens', folded, to, toArrayOf)
import Data.Tuple (snd)
import PureScript.CST.RecordLens (_name, _value)
import PureScript.CST.Types (DelimitedNonEmpty, Labeled, Name, OneOrDelimited(..), Separated, Wrapped)
import PureScript.CST.Types.Lens (_Labeled, _Name, _Separated, _Wrapped)

_WrappedVals :: forall a. Lens' (Wrapped a) a
_WrappedVals = _Wrapped <<< _value

_LabeledVals :: forall r a b. Fold' r (Labeled a b) { label :: a, value :: b }
_LabeledVals = _Labeled <<< to \{ label, value } -> { label, value }

_SeparatedVals :: forall r a. Fold' r (Separated a) (Array a)
_SeparatedVals = _Separated <<< to (\{ head, tail } -> Array.cons head $ map snd tail)

_DelimitedNonEmptyVals :: forall r a. Fold' r (DelimitedNonEmpty a) (Array a)
_DelimitedNonEmptyVals = _WrappedVals <<< _SeparatedVals

_NameVal :: forall a. Lens' (Name a) a
_NameVal = _Name <<< _name

_OneOrDelimitedVals :: forall r a. Fold' r (OneOrDelimited a) (Array a)
_OneOrDelimitedVals = to case _ of
  One a -> [a]
  Many delNE -> toArrayOf (_DelimitedNonEmptyVals <<< folded) delNE
