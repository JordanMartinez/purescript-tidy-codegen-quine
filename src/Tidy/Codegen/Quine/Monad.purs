module Tidy.Codegen.Quine.Monad where

import Prelude

import Control.Monad.Free (Free)
import Control.Monad.State (class MonadState, StateT(..), modify_, runStateT)
import Data.Array as Array
import Data.Identity (Identity)
import Data.Map (Map)
import Data.Tuple (Tuple(..))
import PureScript.CST.Types (Binder, DoStatement)
import Tidy.Codegen (binderVar, doBind, doDiscard, exprApp, exprOp, exprString)
import Tidy.Codegen.Monad (CodegenT, importFrom, importOp, importValue)

type QuineState e m =
  { imports :: Array (DoStatement e)
  , declarations :: Array (DoStatement e)
  , identifiers :: Map String (Map Int (CodegenT e m Unit))
  , exportsAllMembers :: Boolean
  }

newtype QuineT e m a = QuineT (StateT (QuineState e m) (CodegenT e m) a)

type Quine e a = QuineT e (Free Identity) a

runQuineT :: forall e m a. QuineT e m a -> StateT (QuineState e m) (CodegenT e m) a
runQuineT (QuineT ma) = ma

codegenQuine :: forall e m a. Monad m => QuineT e m a -> QuineState e m -> CodegenT e m (Array (DoStatement e))
codegenQuine q s = do
  Tuple _ { imports, declarations } <- runStateT (runQuineT q) s
  pure $ imports <> declarations

liftCodegen :: forall e m a. Monad m => CodegenT e m a -> QuineT e m a
liftCodegen ma = QuineT $ StateT \s -> map (flip Tuple s) ma

derive instance Monad m => Functor (QuineT e m)

instance Monad m => Apply (QuineT e m) where
  apply (QuineT smf) (QuineT sma) = QuineT $ StateT \s -> do
    Tuple f s1 <- runStateT smf s
    Tuple a s2 <- runStateT sma s1
    pure $ Tuple (f a) s2

instance Monad m => Applicative (QuineT e m) where
  pure a = QuineT $ StateT \s -> pure (Tuple a s)

instance Monad m => Bind (QuineT e m) where
  bind (QuineT sma) f = QuineT $ StateT \s -> do
    Tuple a s2 <- runStateT sma s
    let QuineT smb = f a
    runStateT smb s2

instance Monad m => Monad (QuineT e m)

instance Monad m => MonadState (QuineState e m) (QuineT e m) where
  state f = QuineT $ StateT \s -> pure $ f s

appendImport :: forall e m. Monad m => DoStatement e -> QuineT e m Unit
appendImport doStatement = modify_ \s -> s { imports = Array.snoc s.imports doStatement }

-- | importOpen "ModName"
doImportOpen:: forall e m. Partial => Monad m => String -> QuineT e m Unit
doImportOpen modName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importOpen: importValue "importOpen"
    }
  appendImport $ doDiscard $ exprApp cg.importOpen [ exprString modName ]


-- | varName <- importFrom "ModName" $ importValue "valueName"
doImportValue :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportValue modName valueName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importValue: importValue "importValue"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importValue [ exprString valueName ]
      ]
  pure bvar

-- | void $ importFrom "ModName" $ importValue "valueName"
doImportModuleAlias :: forall e m. Partial => Monad m => String -> String -> QuineT e m Unit
doImportModuleAlias modName alias = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importValue: importValue "importValue"
    }
  prelude <- liftCodegen $ importFrom "Prelude"
    { dollar: importOp "$"
    , void: importValue "void"
    }
  appendImport $ doDiscard $
    exprOp prelude.void
      [ prelude.dollar.binaryOp $ exprApp cg.importFrom [ exprString modName ]
      , prelude.dollar.binaryOp $ exprApp cg.importValue [ exprString $ alias <> ".ignored" ]
      ]

-- | varName <- importFrom "ModName" $ importOp "operator"
doImportOp :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportOp modName opName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importOp: importValue "importOp"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importOp [ exprString opName ]
      ]
  pure bvar

-- | varName <- importFrom "ModName" $ importType "Type"
doImportType :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportType modName tyName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importType: importValue "importType"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importType [ exprString tyName ]
      ]
  pure bvar

-- | varName <- importFrom "ModName" $ importTypeAll "TypeName"
doImportTypeAll :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportTypeAll modName tyName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importTypeAll: importValue "importTypeAll"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importTypeAll [ exprString tyName ]
      ]
  pure bvar

-- | varName <- importFrom "ModName" $ importOp "operator"
doImportTypeOp :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportTypeOp modName opName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importTypeOp: importValue "importTypeOp"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importTypeOp [ exprString opName ]
      ]
  pure bvar

-- | varName <- importFrom "ModName" $ importClass "ClassName"
doImportClass :: forall e m. Partial => Monad m => String -> String -> String -> QuineT e m (Binder e)
doImportClass modName opName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importClass: importValue "importClass"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importClass [ exprString opName ]
      ]
  pure bvar

-- | varName <- importFrom "ModName" $ importCtor "TypeName" "ConstructorName"
doImportCtor :: forall e m. Partial => Monad m => String -> String -> String -> String -> QuineT e m (Binder e)
doImportCtor modName tyName ctorName varName = do
  cg <- liftCodegen $ importFrom "Tidy.Codegen.Monad"
    { importFrom: importValue "importFrom"
    , importCtor: importValue "importCtor"
    }
  dollar <- liftCodegen $ importFrom "Prelude" $ importOp "$"
  let bvar = binderVar varName
  appendImport $ doBind bvar $
    exprOp (exprApp cg.importFrom [ exprString modName ])
      [ dollar.binaryOp $ exprApp cg.importCtor [ exprString tyName, exprString ctorName ]
      ]
  pure bvar
