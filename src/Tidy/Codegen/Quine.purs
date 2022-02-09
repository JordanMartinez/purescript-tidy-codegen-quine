module Tidy.Codegen.Quine where

import Prelude

import Control.Monad.Writer (tell)
import Data.Map as Map
import Data.Maybe (isNothing)
import Partial.Unsafe (unsafePartial)
import PureScript.CST.Types (Expr, Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..))
import Tidy.Codegen (declSignature, declValue, exprApp, exprDo, exprIdent, exprOp, exprString, exprWhere, letValue, typeApp)
import Tidy.Codegen.Monad (codegenModule, importCtor, importFrom, importOp, importOpen, importType, importValue)
import Tidy.Codegen.Quine.Monad (Quine, codegenQuine, liftCodegen)
import Tidy.Codegen.Quine.Utils (exprApp1)

genModule :: String -> Module Void -> Module Void
genModule filePath (Module
  { header: ModuleHeader
      { name: Name { name: ModuleName originalModuleName }
      , exports: originalExports
      , imports: originalImports
      }
  , body: ModuleBody { decls: originalDeclarations }
  }) = unsafePartial $ codegenModule (originalModuleName <> ".Generate") do
    importOpen "Prelude"
    prelude <- importFrom "Prelude"
      { tyUnit: importType "Unit"
      , opDollar: importOp "$"
      }
    tyEffect <- importFrom "Effect" $ importType "Effect"
    fnLaunchAff_ <- importFrom "Effect.Aff" $ importValue "launchAff_"
    writeTextFile_ <- importFrom "Node.FS.Aff" $ importValue "writeTextFile"
    utf8_ <- importFrom "Node.Encoding" $ importCtor "Encoding" "UTF8"

    unsafePartial_ <- importFrom "Partial.Unsafe" $ importValue "unsafePartial"
    printModule_ <- importFrom "Tidy.Codegen" $ importValue "printModule"
    codegenModule_ <- importFrom "Tidy.Codegen.Monad" $ importValue "codegenModule"

    generatedDoBlock <- codegenQuine genDoBlock
      { imports: []
      , declarations: []
      , identifiers: Map.empty
      , exportsAllMembers: isNothing originalExports
      }

    tell
      [ declSignature mainFnName $ tyEffect `typeApp` [ prelude.tyUnit ]
      , declValue mainFnName [] do
          exprWhere
            ( exprApp fnLaunchAff_
                [ exprDo [] $
                    exprApp writeTextFile_
                      [ utf8_
                      , exprString filePath
                      , exprIdent generatedMod
                      ]
                ]
            )
            [ letValue generatedMod [] $
                    exprOp printModule_
                      [ prelude.opDollar.binaryOp unsafePartial_
                      , prelude.opDollar.binaryOp $ codegenModule_ `exprApp`
                          [ exprString originalModuleName
                          , generatedDoBlock
                          ]
                      ]
            ]
      ]

  where
  mainFnName = "main"
  generatedMod = "generatedMod"

  genDoBlock :: Partial => Quine Void (Expr Void)
  genDoBlock = do
    prelude <- liftCodegen $ importFrom "Prelude"
      { pureFn: importValue "pure"
      , unitVal: importValue "unit"
      }

    pure $ exprApp1 prelude.pureFn prelude.unitVal
