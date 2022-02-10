module Main where

import Prelude

import ArgParse.Basic (ArgError(..), ArgErrorMsg(..), printArgError)
import CLI (PrefixedGlob, parseCliArgs)
import Data.Array as Array
import Data.Array.NonEmpty as NEA
import Data.Either (Either(..))
import Data.Foldable (foldl, for_, surround)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set as Set
import Data.String as String
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (throw)
import Effect.Ref as Ref
import MkDir (mkdirRecAff)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Stats as Stats
import Node.Glob.Basic (expandGlobsWithStatsCwd)
import Node.Path as Path
import Node.Process (argv)
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..), parseModule)
import Tidy.Codegen (printModule)
import Tidy.Codegen.Quine as Quine
import Types (FileInfo)

main :: Effect Unit
main = do
  args <- map (Array.drop 2) argv
  case parseCliArgs args of
    Left e -> do
      log $ printArgError e
      case e of
        ArgError _ ShowHelp ->
          Process.exit 0
        ArgError _ (ShowInfo _) ->
          Process.exit 0
        _ ->
          Process.exit 1
    Right { prefixedGlobs, options } -> launchAff_ do
      files <- globsToFileInfo options.generatorDir options.genOutputDir prefixedGlobs
      for_ files \{ inputFile, tidyCodegenFile, generatedOutputFile } -> do
        content <- FSA.readTextFile UTF8 inputFile
        case parseModule content of
          ParseSucceeded mod -> do
            mkdirRecAff $ Path.dirname tidyCodegenFile
            FSA.writeTextFile UTF8 tidyCodegenFile $ printModule $ Quine.genModule generatedOutputFile Nothing mod
          _ -> log "Error when parsing file..."

globsToFileInfo :: String -> String -> Array PrefixedGlob -> Aff (Array FileInfo)
globsToFileInfo generatorDir genOutputDir = map join <<< traverse \{ glob, dirCount } -> do
  globFiles <- expandGlobs [ glob ]

  outputFilesRef <- liftEffect $ Ref.new Map.empty

  fileInfo <- for globFiles \file -> do
    let
      originalDirPlusFile = Path.concat
        [ Path.dirname file
        , Path.basenameWithoutExt file (Path.extname file)
        ]
      -- Per the Node docs
      -- (https://nodejs.org/api/path.html#pathrelativefrom-to),
      -- if an empty string is passed in as the 'from'
      -- then the current working directory will be used.
      relativePath = Path.relative "" originalDirPlusFile
      tidyCodegenFile = Path.concat
        $ Array.cons generatorDir
        $ flip Array.snoc "Generate.purs"
        $ Array.drop dirCount
        $ String.split (String.Pattern Path.sep) relativePath

      generatedOutputFile = Path.concat
        $ Array.cons genOutputDir
        $ Array.drop dirCount
        $ String.split (String.Pattern Path.sep)
        $ Path.relative "" file

    liftEffect $ flip Ref.modify_ outputFilesRef \m ->
      Map.alter
        case _ of
          Nothing -> Just $ Set.singleton file
          Just prev -> Just $ Set.insert file prev
        tidyCodegenFile
        m
    pure { inputFile: file, tidyCodegenFile, generatedOutputFile }

  liftEffect do
    outputtedFiles <- Ref.read outputFilesRef
    for_ (NEA.fromArray $ Map.toUnfoldable $ Map.filter (\r -> Set.size r /= 1) outputtedFiles) \nea -> do
      throw
        $ String.joinWith ""
            [ "Error in source glob: '"
            , glob
            , "'. Error in DIR_STRIP_COUNT arg: '"
            , show dirCount
            , "'. Found a case where the following input files' generated optics content would be "
            , "outputted into the same file. Later file's output would overwrite earlier file's output. "
            , "To fix, decrease the DIR_STRIP_COUNT part of the source glob."
            , foldl (<>) "\n" $ nea <#> \(Tuple outFile inputFiles) -> String.joinWith "\n"
                [ "Output file: "
                , "\t" <> outFile
                , append "Input files:" $ surround "\n\t" inputFiles
                ]
            ]

  pure fileInfo

-- Note: credit for the below code goes to @natefaubion
-- since this was copied from `natefaubion/purescript-tidy`
expandGlobs :: Array String -> Aff (Array String)
expandGlobs = map dirToGlob >>> expandGlobsWithStatsCwd >>> map onlyFiles
  where
  dirToGlob path =
    if Path.extname path == "" then
      if isJust (String.stripSuffix (String.Pattern "**") path) then
        Path.concat [ path, "*.purs" ]
      else
        Path.concat [ path, "**", "*.purs" ]
    else
      path

  onlyFiles =
    Map.filter Stats.isFile
      >>> Map.keys
      >>> Set.toUnfoldable
