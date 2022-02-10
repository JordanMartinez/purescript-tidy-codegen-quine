module Test.Snapshots.DataDecls.Generate where

import Prelude

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
    pure unit
