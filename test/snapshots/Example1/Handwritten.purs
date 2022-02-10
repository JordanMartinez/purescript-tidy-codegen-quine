module Test.Snapshots.Example1.Handwritten where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (binaryOp, declSignature, declValue, doDiscard, exprApp, exprDo, exprIdent, exprOp, exprString, printModule, typeApp, typeCtor)
import Tidy.Codegen.Monad (codegenModule, importFrom, importOpen, importType, importValue)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8 "test/snapshots/Example1/TidyCodegen.purs" generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.Example1.Generated" do
    importOpen "Prelude"
    ty_Effect <- importFrom "Effect" $ importType "Effect"
    fn_launchAff_ <- importFrom "Effect.Aff" $ importValue "launchAff_"
    fn_log <- importFrom "Effect.Class.Console" $ importValue "log"
    tell
      [ declSignature "main" $ typeApp ty_Effect [ typeCtor "Unit" ]
      , declValue "main" [] do
          exprApp fn_launchAff_
            [ exprDo
              [ doDiscard $ exprApp fn_log [ exprString "This is some text I'm logging to the console" ]
              , doDiscard $ exprApp (exprIdent "pure") [ (exprIdent "unit") ]
              ]
              $ exprOp fn_log
                  [ binaryOp "$" $ exprApp (exprIdent "show") [ exprIdent "unit" ]
                  ]
            ]
      ]
