module Test.Snapshots.Example1.Generate where

import Prelude

import Control.Monad.Writer (tell)
import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (declSignature, printModule, typeApp, typeCtor)
import Tidy.Codegen.Monad (codegenModule)

main :: Effect Unit
main = launchAff_ do
  writeTextFile UTF8
    "/home/jordan/Programming/Projects/tidy-codegen-quine/test/snapshots/Example1/Generated.purs"
    generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.Example1.Generated" do
    do
      tell
        [ declSignature "main"
            (typeApp (typeCtor "Requires importing type") [ typeCtor "Requires importing type" ])
        , "This declaration is not yet supported..."
        ]
