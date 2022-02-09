module Test.Main where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (for_)
import Data.Lens (view)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_, makeAff, message, nonCanceler)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Ref as Ref
import Node.ChildProcess (ExecOptions, ExecResult, defaultExecOptions)
import Node.ChildProcess as ChildProcess
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FSA
import Node.FS.Stats (Stats)
import Node.FS.Stats as Stats
import Node.Glob.Basic (expandGlobsWithStatsCwd)
import Node.Path (FilePath)
import Node.Path as Path
import Node.Process as Process
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.RecordLens (_header, _name)
import PureScript.CST.Types.Lens (_Module, _ModuleHeader, _ModuleName, _Name)
import Tidy.Codegen (printModule)
import Tidy.Codegen.Quine as Quine

main :: Effect Unit
main = launchAff_ do
  invalidGenerations <- liftEffect $ Ref.new []
  files <- map onlyFiles $ expandGlobsWithStatsCwd [ "test/snapshots/*.purs" ]
  for_ files \filePath -> do
    let
      bname = Path.basenameWithoutExt filePath (Path.extname filePath)
      tidyCodegenFile = Path.concat [ Path.dirname filePath, bname , "TidyCodegen.purs" ]
      genOutputFile = Path.concat [ Path.dirname filePath, bname , "Generated.purs" ]
    log $ "Testing '" <> bname <> "'..."
    fileContent <- FSA.readTextFile UTF8 filePath
    case parseModule fileContent of
      ParseSucceeded mod -> do
        FSA.writeTextFile UTF8 tidyCodegenFile $ printModule $ Quine.genModule genOutputFile mod
        let
          origModName = view (_Module <<< _header <<< _ModuleHeader <<< _name <<< _Name <<< _name <<< _ModuleName) mod
          tidyCodegenMod = origModName <> ".Generate"
        res <- execAsync ("spago test -m " <> tidyCodegenMod) $ defaultExecOptions { cwd = Just "." }
        case res.error of
          Just e -> do
            liftEffect $ flip Ref.modify_ invalidGenerations \arr ->
                Array.snoc arr $ Tuple bname $ "Error thrown: " <> message e
          Nothing -> do
            genContent <- FSA.readTextFile UTF8 genOutputFile
            when (fileContent /= genContent) do
              liftEffect $ flip Ref.modify_ invalidGenerations \arr ->
                Array.snoc arr $ Tuple bname "Original file did not match generated one"
      _ -> log "Error when parsing file..."
  invalid <- liftEffect $ Ref.read invalidGenerations
  for_ invalid \(Tuple bname err) -> do
    log $ bname <> ": " <> err
  unless (Array.null invalid) $ liftEffect $ Process.exit 1

onlyFiles :: Map String Stats -> Array FilePath
onlyFiles =
  Map.filter Stats.isFile
    >>> Map.keys
    >>> Set.toUnfoldable

execAsync :: String -> ExecOptions -> Aff ExecResult
execAsync command options = makeAff \cb -> do
  _ <- ChildProcess.exec command options \execResult ->
    cb (Right execResult)
  pure nonCanceler
