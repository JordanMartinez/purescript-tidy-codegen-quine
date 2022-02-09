module Test.Snapshots.Example1.Generate where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (writeTextFile)
import Partial.Unsafe (unsafePartial)
import Tidy.Codegen (printModule)
import Tidy.Codegen.Monad (codegenModule)

main :: Effect Unit
main = launchAff_ do writeTextFile UTF8 "test/snapshots/Example1/Example1.purs" generatedMod
  where
  generatedMod = printModule $ unsafePartial $ codegenModule "Test.Snapshots.Example1" do pure unit
