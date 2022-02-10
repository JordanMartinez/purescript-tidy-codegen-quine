module Tidy.Codegen.Quine.LensUtils where

import Prelude
import Prim hiding (Type, Row)

import Data.Array as Array
import Data.Lens (Fold', Lens', _1, _2, _Just, folded, preview, previewOn, to, toArrayOf, toArrayOfOn)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (snd)
import PureScript.CST.RecordLens (_header, _name, _value)
import PureScript.CST.Types (DelimitedNonEmpty, Ident, Instance, InstanceBinding, Label, Labeled, Module, Name, OneOrDelimited(..), Proper, QualifiedName, Row, Separated, Type, Wrapped)
import PureScript.CST.Types.Lens (_Instance, _Labeled, _Module, _ModuleHeader, _ModuleName, _Name, _QualifiedName, _Row, _Separated, _Wrapped)

_ModuleNameFull :: forall e. Lens' (Module e) String
_ModuleNameFull = _Module <<< _header <<< _ModuleHeader <<< _name <<< _NameVal <<< _ModuleName

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

_RowVal :: forall e r. Fold' r (Row e) { labels :: Array { label :: Name Label, value :: Type e }, tail :: Maybe (Type e) }
_RowVal = _Row <<< to \rec ->
  { labels: toArrayOfOn rec.labels (_Just <<< _SeparatedVals <<< folded <<< _LabeledVals)
  , tail: previewOn rec.tail (_Just <<< _2)
  }

_InstanceVal
  :: forall e r
   . Fold'
      r
      (Instance e)
      { head ::
          { name :: Maybe (Name Ident)
          , constraints :: Array (Type e)
          , className :: QualifiedName Proper
          , types :: Array (Type e)
          }
      , body :: Array (InstanceBinding e)
      }
_InstanceVal = _Instance <<< to \{ head, body } ->
  { head:
      { name: previewOn head.name (_Just <<< _1)
      , constraints: toArrayOfOn head.constraints (_Just <<< _1 <<< _OneOrDelimitedVals <<< folded)
      , className: head.className
      , types: head.types
      }
  , body: toArrayOfOn body (_Just <<< _2 <<< folded)
  }