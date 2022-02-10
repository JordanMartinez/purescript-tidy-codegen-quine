module Test.Snapshots.DataDecls.Generate where

import Prelude

import Control.Monad.Writer (tell)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (dataCtor, declData, declDataSignature, declNewtype, declType, printModule, typeApp, typeArrow, typeForall, typeParens, typeRecord, typeVar, typeVarKinded)
import Tidy.Codegen.Monad (codegenModule, importFrom, importType)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8
    "/home/jordan/Programming/Projects/tidy-codegen-quine/test/snapshots/DataDecls/Generated.purs"
    generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.DataDecls.Generated" do
    varName <- importFrom "Some.Module" $ importType "Int"
    varName <- importFrom "Some.Module" $ importType "Int"
    varName <- importFrom "Some.Module" $ importType "TypeAlias"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "DTyVars"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "DTyVars"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Type"
    varName <- importFrom "Some.Module" $ importType "Tuple"
    varName <- importFrom "Some.Module" $ importType "Complex"
    do
      varName <- importFrom "Some.Module" $ importType "Int"
      varName <- importFrom "Some.Module" $ importType "Int"
      varName <- importFrom "Some.Module" $ importType "TypeAlias"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "DTyVars"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "DTyVars"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Type"
      varName <- importFrom "Some.Module" $ importType "Tuple"
      varName <- importFrom "Some.Module" $ importType "Complex"
      tell
        [ declData "NoCtors" [] []
        , declData "OneCtor" [] [ dataCtor "OneCtor" [] ]
        , declData "TwoCtors" [] [ dataCtor "Ctor1" [], dataCtor "Ctor2" [] ]
        , declNewtype "NType" [] "NType" varName
        , declType "TypeAlias" [] (typeRecord [ Tuple "a" varName ] Nothing)
        , declNewtype "NTypeAlias" [] "NTypeAlias" varName
        , declData "DTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" varName ]
            [ dataCtor "DTyVars" [ typeVar "a", typeVar "b", typeVar "c" ] ]
        , declNewtype "NTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" varName ] "NTyVars"
            (typeParens (typeApp varName [ typeVar "a", typeVar "b", typeVar "c" ]))
        , declType "TTyVars" [ typeVar "a", typeVar "b", typeVarKinded "c" varName ]
            (typeApp varName [ typeVar "a", typeVar "b", typeVar "c" ])
        , declDataSignature "Complex"
            ( typeArrow [ varName ]
                ( typeArrow [ typeParens (typeArrow [ varName ] varName) ]
                    (typeArrow [ varName ] varName)
                )
            )
        , declData "Complex" [ typeVar "a", typeVar "b", typeVar "c" ]
            [ dataCtor "C1" []
            , dataCtor "C2" [ typeVar "a" ]
            , dataCtor "C3" [ typeParens (typeApp (typeVar "b") [ typeVar "c" ]) ]
            , dataCtor "C4"
                [ typeParens
                    (typeApp varName [ typeVar "a", typeParens (typeApp (typeVar "b") [ typeVar "c" ]) ])
                ]
            , dataCtor "C5" [ typeRecord [ Tuple "foo" (typeVar "a") ] Nothing ]
            , dataCtor "C6" [ typeParens (typeApp varName [ typeVar "a", typeVar "b", typeVar "c" ]) ]
            , dataCtor "C7"
                [ typeParens (typeForall [ typeVar "n" ] (typeArrow [ typeVar "n" ] (typeVar "a"))) ]
            ]
        ]
