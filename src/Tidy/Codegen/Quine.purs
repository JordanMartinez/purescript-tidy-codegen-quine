module Tidy.Codegen.Quine where

import Prelude
import Prim hiding (Type)

import Control.Monad.Writer (tell)
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Foldable (for_)
import Data.Lens (_1, _Just, folded, preview, toArrayOf, toArrayOfOn, view, viewOn)
import Data.Lens.Lens.Tuple (_2)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CST.RecordLens (_value)
import PureScript.CST.Types (ClassFundep, DataCtor(..), Declaration(..), Expr, Fixity(..), FixityOp(..), Foreign(..), Ident, Labeled, Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Role(..), Type(..), TypeVarBinding(..))
import PureScript.CST.Types.Lens (_Ident, _Operator, _Proper, _SourceToken, _TokLowerName)
import Tidy.Codegen (declSignature, declValue, exprApp, exprArray, exprDo, exprIdent, exprInt, exprOp, exprString, exprWhere, letValue, typeApp)
import Tidy.Codegen.Monad (codegenModule, importCtor, importFrom, importOp, importOpen, importType, importValue)
import Tidy.Codegen.Quine.LensUtils (_LabeledVals, _NameVal, _OneOrDelimitedVals, _QualifiedNameVal, _SeparatedVals, _WrappedVals)
import Tidy.Codegen.Quine.Monad (Quine, codegenQuine, liftCodegen)
import Tidy.Codegen.Quine.Utils (exprApp1)

genModule :: String -> Maybe ModuleName -> Module Void -> Module Void
genModule filePath outModName (Module
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
    ctorUtf8_ <- importFrom "Node.Encoding" $ importCtor "Encoding" "UTF8"

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
                      [ ctorUtf8_
                      , exprString filePath
                      , exprIdent generatedMod
                      ]
                ]
            )
            [ letValue generatedMod [] $
                    exprOp printModule_
                      [ prelude.opDollar.binaryOp unsafePartial_
                      , prelude.opDollar.binaryOp $ codegenModule_ `exprApp`
                          [ exprString $ maybe originalModuleName unwrap outModName
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
    for_ originalDeclarations case _ of
      -- DeclData (DataHead e) (Maybe (Tuple SourceToken (Separated (DataCtor e))))
      DeclData { name, vars } mbCtors -> do
        -- declData "name" [ tyVar "var", tyVarKinded "var" (typeCtor "Type") ] $ fromMaybe [] ctors
        --   where ctors = [ dataCtor "Name" [], dataCtor "Bar" [ typeCtor "Type" ] ]
        generatedTyVars <- traverse genTyVar vars
        generatedCtors <- genCtors $ toArrayOf (_Just <<< _2 <<< _SeparatedVals <<< folded) mbCtors
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declData: importValue "declData"
          }
        pure $ exprApp cg.declData
          [ exprString $ view (_NameVal <<< _Proper) name
          , exprArray generatedTyVars
          , exprArray generatedCtors
          ]

      -- DeclType (DataHead e) SourceToken (Type e)
      DeclType { name, vars } _ ty -> do
        -- declType "name" [ tyVar "foo", typeVarKinded "foo" (typeCtor "Bar") ] (typeCtor "Baz")
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declType: importValue "declType"
          }
        generatedTyVars <- traverse genTyVar vars
        generatedTy <- genType ty
        pure $ exprApp cg.declType
          [ exprString $ view (_NameVal <<< _Proper) name
          , exprArray generatedTyVars
          , generatedTy
          ]

      -- DeclNewtype (DataHead e) SourceToken (Name Proper) (Type e)
      DeclNewtype { name, vars } _ ctor ty -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declNewtype: importValue "declNewtype"
          }
        generatedTyVars <- traverse genTyVar vars
        generatedTy <- genType ty
        pure $ exprApp cg.declNewtype
          [ exprString $ view (_NameVal <<< _Proper) name
          , exprArray generatedTyVars
          , exprString $ view (_NameVal <<< _Proper) ctor
          , generatedTy
          ]

      -- DeclClass (ClassHead e) (Maybe (Tuple SourceToken (NonEmptyArray (Labeled (Name Ident) (Type e))))) ->
      DeclClass { super, name, vars, fundeps } members -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declClass: importValue "declClass"
          }
        generatedSuper <- traverse genType $ toArrayOf (_Just <<< _1 <<< _OneOrDelimitedVals <<< folded) super
        generatedTyVars <- traverse genTyVar vars
        generatedFunDeps <- traverse genFunDep $ toArrayOf (_Just <<< _2 <<< _SeparatedVals <<< folded) fundeps
        generatedMembers <- traverse genClassMember $ toArrayOf (_Just <<< _2 <<< folded) members
        pure $ exprApp cg.declClass
          [ exprArray generatedSuper
          , exprString $ view (_NameVal <<< _Proper) name
          , exprArray generatedTyVars
          , exprArray generatedFunDeps
          , exprArray generatedMembers
          ]

      -- DeclInstanceChain (Separated (Instance e))
      -- DeclInstanceChain sep -> do

      -- DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
      DeclDerive _ _ { name, constraints, className, types } -> do
        -- declDerive Nothing [ typeCtor "Constraint" ] "ClassName" [ typCtor "Type" ]
        maybeName <- case preview (_Just <<< _1 <<< _NameVal <<< _Ident) name of
          Nothing -> do
            liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Nothing"
          Just instName -> do
            ctor_Just <- liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Just"
            pure $ exprApp ctor_Just [ exprString instName ]
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declDerive: importValue "declDerive"
          }
        generatedConstraints <- traverse genType $ toArrayOfOn constraints (_Just <<< _1 <<< _OneOrDelimitedVals <<< folded)
        generatedTypes <- traverse genType types
        pure $ exprApp cg.declDerive
          [ maybeName
          , exprArray generatedConstraints
          , exprString $ viewOn className (_QualifiedNameVal (view _Proper))
          , exprArray generatedTypes
          ]

      -- DeclKindSignature SourceToken (Labeled (Name Proper) (Type e))
      DeclKindSignature keyword lbld -> do
        let
          { label, value } = view _LabeledVals lbld
        fn_declKeywordSignature <- liftCodegen
          $ importFrom "Tidy.Codegen"
          $ importValue
          case preview (_SourceToken <<< _value <<< _TokLowerName <<< _2) keyword of
            Just "data" -> "declDataSignature"
            Just "newtype" -> "declNewtypeSignature"
            Just "type" -> "declTypeSignature"
            _ -> unsafeCrashWith "Invalid decl kind signature keyword"
        generatedType <- genType value
        pure $ exprApp fn_declKeywordSignature
          [ exprString $ view (_NameVal <<< _Proper) label
          , generatedType
          ]

      -- DeclSignature (Labeled (Name Ident) (Type e))
      DeclSignature lbld -> do
        let
          { label, value } = view _LabeledVals lbld
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { declSignature: importValue "declSignature"
          }
        generatedType <- genType value
        pure $ exprApp cg.declSignature
          [ exprString $ view (_NameVal <<< _Ident) label
          , generatedType
          ]

      -- DeclValue (ValueBindingFields e)

      -- DeclFixity FixityFields
      DeclFixity { keyword, operator, prec } -> do
        -- declInfix Infix 4 "map" "<$>"
        -- declInfix (snd keyword) (snd prec) case operator of
        case operator of
          -- FixityValue (QualifiedName (Either Ident Proper)) SourceToken (Name Operator)
          FixityValue qual _ op -> do
            cg <- liftCodegen $ importFrom "Tidy.Codegen"
              { declInfix: importValue "declInfix"
              }
            ctor_Infix <- liftCodegen $ importFrom "PureScript.CST.Types"
              $ importCtor "Fixity" case snd keyword of
                  Infix -> "Infix"
                  Infixl -> "Infixl"
                  Infixr -> "Infixr"

            pure $ exprApp cg.declInfix
              [ ctor_Infix
              , exprInt $ snd prec
              , exprString $ viewOn qual (_QualifiedNameVal (either (view _Ident) (view _Proper)))
              , exprString $ viewOn op (_NameVal <<< _Operator)
              ]
          -- FixityType SourceToken (QualifiedName Proper) SourceToken (Name Operator)
          FixityType _ qual _ op -> do
            cg <- liftCodegen $ importFrom "Tidy.Codegen"
              { declInfixType: importValue "declInfixType"
              }
            ctor_Infix <- liftCodegen $ importFrom "PureScript.CST.Types"
              $ importCtor "Fixity" case snd keyword of
                  Infix -> "Infix"
                  Infixl -> "Infixl"
                  Infixr -> "Infixr"

            pure $ exprApp cg.declInfixType
              [ ctor_Infix
              , exprInt $ snd prec
              , exprString $ viewOn qual (_QualifiedNameVal (view _Proper))
              , exprString $ viewOn op (_NameVal <<< _Operator)
              ]

      -- DeclForeign SourceToken SourceToken (Foreign e)
      DeclForeign _ _ forgn -> case forgn of
        -- ForeignValue (Labeled (Name Ident) (Type e)) ->
        ForeignValue lbld -> do
          -- declForeign "name" ty
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
              { declForeign: importValue "declForeign"
              }
          let { label, value } = view _LabeledVals lbld
          generatedType <- genType value
          pure $ exprApp cg.declForeign
            [ exprString $ view (_NameVal <<< _Ident) label
            , generatedType
            ]

        -- ForeignData SourceToken (Labeled (Name Proper) (Type e)) ->
        ForeignData _ lbld -> do
          -- declForeignData "name" ty
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
              { declForeignData: importValue "declForeignData"
              }
          let { label, value } = view _LabeledVals lbld
          generatedType <- genType value
          pure $ exprApp cg.declForeignData
            [ exprString $ view (_NameVal <<< _Proper) label
            , generatedType
            ]

        -- ForeignKind SourceToken (Name Proper) ->
        ForeignKind _ _proper ->
          pure $ exprString "ForeignKind is deprecated syntax"

      -- DeclRole SourceToken SourceToken (Name Proper) (NonEmptyArray (Tuple SourceToken Role))
      DeclRole _ _ name roles -> do
        -- declRole "name" roles
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
              { declRole: importValue "declRole"
              }
        cst <- liftCodegen $ importFrom "PureScript.CST.Types"
              { phantom: importCtor "Role" "Phantom"
              , nominal: importCtor "Role" "Nominal"
              , representational: importCtor "Role" "Representational"
              }
        let
          genRole = case _ of
            Phantom -> cst.phantom
            Nominal -> cst.nominal
            Representational -> cst.representational
          generatedRoles = map (genRole <<< snd) $ NEA.toArray roles
        pure $ exprApp cg.declRole
          [ exprString $ view (_NameVal <<< _Proper) name
          , exprArray generatedRoles
          ]

      DeclError e ->
        absurd e

      _ ->
        pure $ exprString "This declaration is not yet supported..."

    prelude <- liftCodegen $ importFrom "Prelude"
      { pureFn: importValue "pure"
      , unitVal: importValue "unit"
      }

    pure $ exprApp1 prelude.pureFn prelude.unitVal

  genCtors :: Partial => Array (DataCtor Void) -> Quine Void (Array (Expr Void))
  genCtors ctorArray = do
    cg <- liftCodegen $ importFrom "Tidy.Codegen"
      { dataCtor: importValue "dataCtor"
      }
    for ctorArray \(DataCtor rec) -> do
      generatedFields <- for rec.fields genType
      pure $ exprApp cg.dataCtor
        [ exprString $ view (_NameVal <<< _Proper) rec.name
        , exprArray generatedFields
        ]

  genTyVar :: Partial => TypeVarBinding Void -> Quine Void (Expr Void)
  genTyVar = case _ of
    TypeVarName n -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeVar: importValue "typeVar"
        }
      pure $ exprApp cg.typeVar
        [ exprString $ view (_NameVal <<< _Ident) n ]
    TypeVarKinded nWithK -> do
      let
        { label, value } = view (_WrappedVals <<< _LabeledVals) nWithK
      generatedType <- genType value
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeVarKinded: importValue "typeVarKinded"
        }
      pure $ exprApp cg.typeVarKinded
        [ exprString $ view (_NameVal <<< _Ident) label
        , generatedType
        ]

  genType :: Partial => Type Void -> Quine Void (Expr Void)
  genType = case _ of
    -- TypeVar (Name Ident)
    TypeVar nameIdent -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeVar: importValue "typeVar"
          }
      pure $ exprApp cg.typeVar
        [ exprString $ view (_NameVal <<< _Ident) nameIdent ]

    -- TypeConstructor (QualifiedName Proper) -> do
    TypeConstructor qualProper -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeCtor: importValue "typeCtor"
          }
      pure $ exprApp cg.typeCtor [ exprString "Requires importing type" ]

    -- TypeWildcard SourceToken -> do
    TypeWildcard _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "typeWildcard"

    -- TypeHole (Name Ident) -> do
    TypeHole _ -> do
      pure $ exprString "Type holes not yet supported in tidy codegen"

    -- TypeString SourceToken String
    TypeString _ str -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeString: importValue "typeString"
          }
      pure $ exprApp cg.typeString
        [ exprString str
        ]

    -- TypeRow (Wrapped (Row e))
    TypeRow wrappedRow -> do
      pure $ exprString "Type Row not supported yet"

    -- TypeRecord (Wrapped (Row e))
    TypeRecord wrappedRow -> do
      pure $ exprString "Type Record not supported yet"

    -- TypeForall SourceToken (NonEmptyArray (TypeVarBinding e)) SourceToken (Type e)
    TypeForall _ tyVars _ ty -> do
      -- typeForall tyvars ty
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeForall: importValue "typeForall"
          }
      pure $ exprString "TypeForall not supported yet"

    -- TypeKinded (Type e) SourceToken (Type e)
    TypeKinded ty _ kind -> do
      -- typeKinded ty kind
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeKinded: importValue "typeKinded"
          }
      generatedType <- genType ty
      generatedKind <- genType kind
      pure $ exprApp cg.typeKinded
        [ generatedType
        , generatedKind
        ]

    -- TypeApp (Type e) (NonEmptyArray (Type e))
    TypeApp ty args -> do
      -- typeApp ty args
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeApp: importValue "typeApp"
          }
      generatedType <- genType ty
      generatedArgs <- traverse genType args
      pure $ exprApp cg.typeApp
        [ generatedType
        , exprArray $ NEA.toArray generatedArgs
        ]

    -- TypeOp (Type e) (NonEmptyArray (Tuple (QualifiedName Operator) (Type e)))
    TypeOp ty binOps -> do
      generatedType <- genType ty
      generatedBinOps <- do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { binaryOp: importValue "binaryOp"
          }
        for binOps \(Tuple qualOp nextTy) -> do
          generatedNextType <- genType nextTy
          pure $ exprApp cg.binaryOp
            [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator))
            , generatedNextType
            ]
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeOp: importValue "typeOp"
          }
      pure $ exprApp cg.typeOp
        [ generatedType
        , exprArray $ NEA.toArray generatedBinOps
        ]

    -- TypeOpName (QualifiedName Operator)
    TypeOpName qualOp -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeOpName: importValue "typeOpName"
          }
      pure $ exprApp cg.typeOpName
        [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator))
        ]

    -- TypeArrow (Type e) SourceToken (Type e)
    TypeArrow left _ right -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeArrow: importValue "typeArrow"
          }
      pure $ exprApp cg.typeArrow
        -- TODO: finish this implementation
        []

    -- TypeArrowName SourceToken
    TypeArrowName _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "typeArrowName"

    -- TypeConstrained (Type e) SourceToken (Type e)
    TypeConstrained args _ rest -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeConstrained: importValue "typeConstrained"
          }
      pure $ exprApp cg.typeConstrained
        -- TODO: finish this implementation
        []

    -- TypeParens (Wrapped (Type e))
    TypeParens wrappedTy -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { typeParens: importValue "typeParens"
          }
      generatedType <- genType $ viewOn wrappedTy (_WrappedVals)
      pure $ exprApp cg.typeParens [ generatedType ]

    -- TypeUnaryRow SourceToken (Type e)
    TypeUnaryRow _ ty -> do
      pure $ exprString "TypeUnaryRow not supported by tidy codegen yet"

    TypeError e ->
      absurd e

  genFunDep :: Partial => ClassFundep -> Quine Void (Expr Void)
  genFunDep = const $ pure $ exprString "genFunDep Not yet implemented"
    -- case _ of
    --   FundepDetermined _ neaNameIdent ->
    --   FundepDetermines from _ to ->

  genClassMember :: Partial => Labeled (Name Ident) (Type Void) -> Quine Void (Expr Void)
  genClassMember lbld = do
    let { label, value } = view _LabeledVals lbld
    cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { classMember: importValue "classMember"
          }
    generatedType <- genType value
    pure $ exprApp cg.classMember
      [ exprString $ view (_NameVal <<< _Ident) label
      , generatedType
      ]
