module CLI where

import Prelude

import ArgParse.Basic (ArgError, anyNotFlag, argument, default, flagHelp, flagInfo, fromRecord, parseArgs, unfolded1, unformat)
import Control.Monad.Error.Class (throwError)
import Data.Array (fold)
import Data.Either (Either)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Node.Path as Path
import Version (version)

type CliArgs =
  { generatorDir :: String
  , genOutputDir :: String
  }

type PrefixedGlob =
  { glob :: String
  , dirCount :: Int
  }

parseCliArgs :: Array String -> Either ArgError { prefixedGlobs :: Array PrefixedGlob, options :: CliArgs }
parseCliArgs =
  parseArgs
    "tidy-quine"
    ( joinWith "\n"
        [ "A CLI for generating the needed `tidy-codegen` code to generate the input file."
        -- , ""
        -- , "Expected usage: "
        -- , ""
        -- , "Examples:"
        ]
    )
    parser
  where
  parser =
    { prefixedGlobs: _, options: _ }
      <$> pursGlobs
      <*> fromRecord
            { generatorDir
            , genOutputDir
            }
      <* flagInfo [ "--version", "-v" ] "Shows the current version" version
      <* flagHelp

  generatorDir =
    argument [ "--generator-dir", "-o" ] "The directory into which to write the generated files (defaults to `generated`)."
      # default "generated"

  genOutputDir =
    argument [ "--generated-output-dir", "-g" ] "When the generated files are run, the directory into which their generated files are outputted."

  pursGlobs =
    anyNotFlag globExample description
      # unformat globExample validate
      # unfolded1
    where
    description = joinWith ""
      [ "Globs for PureScript sources (e.g. `src` `test/**/*.purs`) "
      , "and the number of root directories to strip from each file path (defaults to 1) "
      , "that are separated by the OS-specific path delimiter (POSIX: ':', Windows: ';'), "
      ]
    delimit l r = l <> Path.delimiter <> r
    globExample = delimit "GLOB[" "DIR_STRIP_COUNT]"
    validate s = do
      case String.split (String.Pattern Path.delimiter) s of
        [ glob, dirStripCount ]
          | Just dirCount <- Int.fromString dirStripCount -> pure { glob, dirCount }
          | otherwise -> throwError $ fold
              [ "Invalid source glob. Expected directory strip count to be an integer "
              , "but was '"
              , s
              , "'"
              ]

        [ glob ] -> pure { glob, dirCount: 1 }
        _ -> throwError $ joinWith ""
          [ "Invalid source glob. Expected either a glob (e.g. `src`) "
          , "or a glob and the prefix to strip separated by a '"
          , Path.delimiter
          , "' character "
          , "(e.g. `"
          , delimit "test/snapshots/**/*.purs" "2"
          , "`)"
          ]
