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
  writeTextFile UTF8 "src/Tidy/Codegen/Quine/Generate.purs" generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Tidy.Codegen.Quine" do
    importOpen "Prelude"
    effectTy <- importFrom "Effect" $ importType "Effect"
    launchAff_Fn <- importFrom "Effect.Aff" $ importValue "launchAff_"
    logFn <- importFrom "Effect.Class.Console" $ importValue "log"
    tell
      [ declSignature "main" $ typeApp effectTy [ typeCtor "Unit" ]
      , declValue "main" [] do
          exprApp launchAff_Fn
            [ exprDo
              [ doDiscard $ exprApp logFn [ exprString "This is some text I'm logging to the console" ]
              , doDiscard $ exprApp (exprIdent "pure") [ (exprIdent "unit") ]
              ]
              $ exprOp logFn
                  [ binaryOp "$" $ exprApp (exprIdent "show") [ exprIdent "unit" ]
                  ]
            ]
      ]
