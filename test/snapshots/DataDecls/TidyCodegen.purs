module Test.Snapshots.DataDecls.Generate where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (dataCtor, declData, declDataSignature, declNewtype, declType, printModule, typeApp, typeArrow, typeCtor, typeForall, typeParens, typeVar, typeVarKinded)
import Tidy.Codegen.Monad (codegenModule)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8
    "/home/jordan/Programming/Projects/tidy-codegen-quine/test/snapshots/DataDecls/Generated.purs"
    generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.DataDecls.Generated" do
    do
      tell
        [ declData "NoCtors" [] []
        , declData "OneCtor" [] [ dataCtor "OneCtor" [] ]
        , declData "TwoCtors" [] [ dataCtor "Ctor1" [], dataCtor "Ctor2" [] ]
        , declNewtype "NType" [] "NType" (typeCtor "Requires importing type")
        , declType "TypeAlias" [] "Type Record not supported yet"
        , declNewtype "NTypeAlias" [] "NTypeAlias" (typeCtor "Requires importing type")
        , declData "DTyVars"
            [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Requires importing type") ]
            [ dataCtor "DTyVars" [ typeVar "a", typeVar "b", typeVar "c" ] ]
        , declNewtype "NTyVars"
            [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Requires importing type") ]
            "NTyVars"
            ( typeParens
                (typeApp (typeCtor "Requires importing type") [ typeVar "a", typeVar "b", typeVar "c" ])
            )
        , declType "TTyVars"
            [ typeVar "a", typeVar "b", typeVarKinded "c" (typeCtor "Requires importing type") ]
            (typeApp (typeCtor "Requires importing type") [ typeVar "a", typeVar "b", typeVar "c" ])
        , declDataSignature "Complex" typeArrow
        , declData "Complex" [ typeVar "a", typeVar "b", typeVar "c" ]
            [ dataCtor "C1" []
            , dataCtor "C2" [ typeVar "a" ]
            , dataCtor "C3" [ typeParens (typeApp (typeVar "b") [ typeVar "c" ]) ]
            , dataCtor "C4"
                [ typeParens
                    ( typeApp (typeCtor "Requires importing type")
                        [ typeVar "a", typeParens (typeApp (typeVar "b") [ typeVar "c" ]) ]
                    )
                ]
            , dataCtor "C5" [ "Type Record not supported yet" ]
            , dataCtor "C6"
                [ typeParens
                    ( typeApp (typeCtor "Requires importing type")
                        [ typeVar "a", typeVar "b", typeVar "c" ]
                    )
                ]
            , dataCtor "C7" [ typeParens "TypeForall not supported yet" ]
            ]
        ]
