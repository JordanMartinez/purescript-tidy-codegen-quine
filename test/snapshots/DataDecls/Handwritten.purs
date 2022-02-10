module Test.Snapshots.DataDecls.Handwritten where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (dataCtor, declData, declDataSignature, declNewtype, declType, printModule, typeApp, typeArrow, typeCtor, typeForall, typeRecord, typeVar, typeVarKinded)
import Tidy.Codegen.Monad (codegenModule, importFrom, importType)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8 "test/snapshots/DataDecls/TidyCodegen.purs" generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.DataDecls.Generated" do
    ty_Tuple <- importFrom "Data.Tuple" $ importType "Tuple"
    tell
      [ declData "NoCtors" [] []
      , declData "OneCtor" []
          [ dataCtor "OneCtor" []
          ]
      , declData "TwoCtors" []
          [ dataCtor "Ctor1" []
          , dataCtor "Ctor2" []
          ]
      , declNewtype "NType" [] "NType" $ typeCtor "Int"
      , declType "TypeAlias" [] $ typeRecord
          [ Tuple "a" $ typeCtor "Int" ]
          Nothing
      , declNewtype "NTypeAlias" [] "NTypeAlias" $ typeCtor "TypeAlias"
      , declData "DTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Type") ]
          [ dataCtor "DTyVars" [ typeVar "a", typeVar "b", typeVar "c" ] ]
      , declNewtype "NTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Type") ]
          "NTyVars" $ typeApp (typeCtor "DTyVars") [ typeVar "a", typeVar "b", typeVar "c" ]
      , declType "TTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Type") ] $
          typeApp (typeCtor "DTyVars") [ typeVar "a", typeVar "b", typeVar "c" ]
      , declDataSignature "Complex" $ typeArrow
          [ typeCtor "Type"
          , typeArrow [ typeCtor "Type" ] $ typeCtor "Type"
          , typeCtor "Type"
          ]
          $ typeCtor "Type"
      , declData "Complex" [ typeVar "a", typeVar "b", typeVar "c" ]
          [ dataCtor "C1" []
          , dataCtor "C2" [ typeVar "a" ]
          , dataCtor "C3" [ typeApp (typeVar "b") [ typeVar "c" ]]
          , dataCtor "C4" [ typeApp ty_Tuple [ typeVar "a", typeApp (typeVar "b") [ typeVar "c" ]]]
          , dataCtor "C5" [ typeRecord [ Tuple "foo" $ typeVar "a" ] Nothing ]
          , dataCtor "C6" [ typeApp (typeCtor "Complex") [ typeVar "a", typeVar "b", typeVar "c" ] ]
          , dataCtor "C7" [ typeForall [ typeVar "n" ] $ typeArrow [ typeVar "n" ] $ typeVar "a" ]
          ]
      ]
