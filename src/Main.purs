module Main where

import Prelude

import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Tidy.Codegen (printModule)
import Tidy.Codegen.Quine as Quine

main :: Effect Unit
main = launchAff_ do
  content <- FSA.readTextFile UTF8 "src/Tidy/Codegen/Quine.purs"
  case parseModule content of
    ParseSucceeded mod -> do
      FSA.writeTextFile UTF8 "generate/Tidy/Codegen/Quine/Generate.purs" $ printModule $ Quine.genModule "src/Tidy/Codegen/Quine/Generate.purs" mod
    _ -> log "Error when parsing file..."
