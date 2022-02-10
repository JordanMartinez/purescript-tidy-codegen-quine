module Tidy.Codegen.Quine.LensUtils where

import Prelude
import Prim hiding (Type)

import Data.Array as Array
import Data.Lens (Fold', Lens', _Just, folded, preview, to, toArrayOf)
import Data.Maybe (maybe)
import Data.Tuple (snd)
import PureScript.CST.RecordLens (_name, _value)
import PureScript.CST.Types (DelimitedNonEmpty, Labeled, Name, OneOrDelimited(..), QualifiedName, Separated, Wrapped)
import PureScript.CST.Types.Lens (_Labeled, _ModuleName, _Name, _QualifiedName, _Separated, _Wrapped)

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

_QualifiedNameVal :: forall r a. (a -> String) -> Fold' r (QualifiedName a) String
_QualifiedNameVal extractName = _QualifiedName <<< to \rec -> do
  let
    modPart = preview (_Just <<< _ModuleName) rec.module
    namePart = extractName rec.name
  maybe namePart (\m -> m <> "." <> namePart) modPart
