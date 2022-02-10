module Test.Snapshots.Example1.Generate where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (binaryOp, declSignature, declValue, doDiscard, exprApp, exprDo, exprIdent, exprOp, exprString, printModule, typeApp)
import Tidy.Codegen.Monad (codegenModule, importFrom, importType)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8
    "/home/jordan/Programming/Projects/tidy-codegen-quine/test/snapshots/Example1/Generated.purs"
    generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.Example1.Generated" do
    varName <- importFrom "Some.Module" $ importType "Effect"
    varName <- importFrom "Some.Module" $ importType "Unit"
    tell
      [ declSignature "main" (typeApp varName [ varName ])
      , declValue "main" []
          ( exprApp (exprIdent "launchAff_")
              [ exprDo
                  [ doDiscard
                      ( exprApp (exprIdent "log")
                          [ exprString "This is some text I'm logging to the console" ]
                      )
                  , doDiscard (exprApp (exprIdent "pure") [ exprIdent "unit" ])
                  ]
                  ( doDiscard
                      ( exprOp (exprIdent "log")
                          [ binaryOp "$" (exprApp (exprIdent "show") [ exprString "foo" ]) ]
                      )
                  )
              ]
          )
      ]
