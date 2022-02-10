module Tidy.Codegen.Quine where

import Prelude
import Prim hiding (Type, Row)

import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Int as Int
import Data.Lens (_1, _Just, folded, preview, to, toArrayOf, toArrayOfOn, view, viewOn)
import Data.Lens.Lens.Tuple (_2)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromJust, isJust, maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CST.RecordLens (_value)
import PureScript.CST.Types (Binder(..), ClassFundep, DataCtor(..), Declaration(..), DoStatement(..), Expr(..), Fixity(..), FixityOp(..), Foreign(..), Guarded(..), GuardedExpr, Ident, Instance, InstanceBinding(..), IntValue(..), Labeled, LetBinding(..), Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), PatternGuard, RecordLabeled(..), RecordUpdate(..), Role(..), Row, Type(..), TypeVarBinding(..), Where, Wrapped)
import PureScript.CST.Types.Lens (_Ident, _Label, _Operator, _Proper, _SourceToken, _TokLowerName)
import Tidy.Codegen (declSignature, declValue, exprApp, exprArray, exprBool, exprChar, exprDo, exprIdent, exprInt, exprIntHex, exprNumber, exprOp, exprString, exprWhere, letValue, typeApp)
import Tidy.Codegen.Monad (codegenModule, importCtor, importFrom, importOp, importOpen, importType, importValue)
import Tidy.Codegen.Quine.LensUtils (_DelimitedNonEmptyVals, _DelimitedVals, _GuardedExprVal, _InstanceVal, _LabeledVals, _NameVal, _OneOrDelimitedVals, _PatternGuardVal, _QualifiedNameVal, _RowVal, _SeparatedVals, _WhereVal, _WrappedVals)
import Tidy.Codegen.Quine.Monad (Quine, codegenQuine, doImportCtor, doImportType, liftCodegen)

genModule :: String -> Maybe ModuleName -> Module Void -> Module Void
genModule
  filePath
  outModName
  ( Module
      { header: ModuleHeader
          { name: Name { name: ModuleName originalModuleName }
          , exports: originalExports
          , imports: originalImports
          }
      , body: ModuleBody { decls: originalDeclarations }
      }
  ) = unsafePartial $ codegenModule (originalModuleName <> ".Generate") do
  importOpen "Prelude"
  prelude <- importFrom "Prelude"
    { ty_Unit: importType "Unit"
    , op_dollar: importOp "$"
    }
  ty_Effect <- importFrom "Effect" $ importType "Effect"
  fn_launchAff_ <- importFrom "Effect.Aff" $ importValue "launchAff_"
  fn_writeTextFile <- importFrom "Node.FS.Aff" $ importValue "writeTextFile"
  ctor_Utf8 <- importFrom "Node.Encoding" $ importCtor "Encoding" "UTF8"

  fn_unsafePartial <- importFrom "Partial.Unsafe" $ importValue "unsafePartial"
  fn_printModule <- importFrom "Tidy.Codegen" $ importValue "printModule"
  fn_codegenModule <- importFrom "Tidy.Codegen.Monad" $ importValue "codegenModule"

  generatedDoBlock <- codegenQuine genDoBlock
    { imports: []
    , identifiers: Map.empty
    }

  tell
    [ declSignature mainFnName $ ty_Effect `typeApp` [ prelude.ty_Unit ]
    , declValue mainFnName [] do
        exprWhere
          ( exprApp fn_launchAff_
              [ exprDo [] $
                  exprApp fn_writeTextFile
                    [ ctor_Utf8
                    , exprString filePath
                    , exprIdent generatedMod
                    ]
              ]
          )
          [ letValue generatedMod [] $
              exprOp fn_printModule
                [ prelude.op_dollar.binaryOp fn_unsafePartial
                , prelude.op_dollar.binaryOp $ fn_codegenModule `exprApp`
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
    decls <- traverse genDeclaration originalDeclarations
    { imports } <- get
    fn_tell <- liftCodegen $ importFrom "Control.Monad.Writer" $ importValue "tell"
    pure
      $ exprDo imports
      $ exprApp fn_tell
          [ exprArray decls
          ]

  genDeclaration :: Partial => Declaration Void -> Quine Void (Expr Void)
  genDeclaration = case _ of
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
    DeclInstanceChain sep -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { declInstanceChain: importValue "declInstanceChain"
        }
      generatedInstances <- genDeclInstances $ toArrayOfOn sep (_SeparatedVals <<< folded)
      pure $ exprApp cg.declInstanceChain
        [ exprArray generatedInstances
        ]

    -- DeclDerive SourceToken (Maybe SourceToken) (InstanceHead e)
    DeclDerive _ _ { name, constraints, className, types } -> do
      -- declDerive Nothing [ typeCtor "Constraint" ] "ClassName" [ typCtor "Type" ]
      maybeName <- (preview (_Just <<< _1 <<< _NameVal <<< _Ident) name)
        # genMaybe \ctor_Just instName -> do
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
        $ case preview (_SourceToken <<< _value <<< _TokLowerName <<< _2) keyword of
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
    -- DeclValue { name :: Name Ident, binders :: Array (Binder e), guarded :: Guarded e }
    DeclValue { name, binders, guarded } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { declValue: importValue "declValue"
        }
      generatedBinders <- traverse genBinder binders
      generatedGuard <- genGuarded guarded
      pure $ exprApp cg.declValue
        [ exprString $ viewOn name (_NameVal <<< _Ident)
        , exprArray generatedBinders
        , generatedGuard
        ]

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

          -- TODO: handle import logic
          -- Because one can define an infix on a value, type, ctor in another file
          -- we still need to handle the import properly here

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

          -- TODO: handle import logic
          -- Because one can define an infix on a value, type, ctor in another file
          -- we still need to handle the import properly here

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
      -- TODO: add import handling logic here...
      doImportType "Some.Module" (viewOn qualProper (_QualifiedNameVal (view _Proper))) "varName"

    -- TypeWildcard SourceToken -> do
    TypeWildcard _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "typeWildcard"

    -- TypeHole (Name Ident) -> do
    TypeHole _ -> do
      -- TODO: update Tidy Codegen to handle this
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
      -- typeRow [ Tuple "labelName" $ typeCtor "TypeName" ] $ Just (typeVar "r")
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeRow: importValue "typeRow"
        }
      { lbls, mbTail } <- genWrappedRow wrappedRow
      pure $ exprApp cg.typeRow
        [ exprArray lbls
        , mbTail
        ]

    -- TypeRecord (Wrapped (Row e))
    TypeRecord wrappedRow -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeRecord: importValue "typeRecord"
        }
      { lbls, mbTail } <- genWrappedRow wrappedRow
      pure $ exprApp cg.typeRecord
        [ exprArray lbls
        , mbTail
        ]

    -- TypeForall SourceToken (NonEmptyArray (TypeVarBinding e)) SourceToken (Type e)
    TypeForall _ tyVars _ ty -> do
      -- typeForall tyvars ty
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeForall: importValue "typeForall"
        }
      generatedTyVars <- traverse genTyVar tyVars
      generatedType <- genType ty
      pure $ exprApp cg.typeForall
        [ exprArray $ NEA.toArray generatedTyVars
        , generatedType
        ]

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
          -- TODO: add import handling logic for qualified name
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
      -- TODO: add import handling logic for qualified name
      pure $ exprApp cg.typeOpName
        [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator))
        ]

    -- TypeArrow (Type e) SourceToken (Type e)
    TypeArrow left _ right -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeArrow: importValue "typeArrow"
        }
      -- Note: not sure if this is correct
      generatedLeft <- genType left
      generatedRight <- genType right
      pure $ exprApp cg.typeArrow
        [ exprArray [ generatedLeft ]
        , generatedRight
        ]

    -- TypeArrowName SourceToken
    TypeArrowName _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "typeArrowName"

    -- TypeConstrained (Type e) SourceToken (Type e)
    TypeConstrained args _ rest -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeConstrained: importValue "typeConstrained"
        }
      -- Note: not sure if this is correct
      generatedArgs <- genType args
      generatedRest <- genType rest
      pure $ exprApp cg.typeConstrained
        [ exprArray [ generatedArgs ]
        , generatedRest
        ]

    -- TypeParens (Wrapped (Type e))
    TypeParens wrappedTy -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { typeParens: importValue "typeParens"
        }
      generatedType <- genType $ viewOn wrappedTy (_WrappedVals)
      pure $ exprApp cg.typeParens [ generatedType ]

    -- TypeUnaryRow SourceToken (Type e)
    TypeUnaryRow _ _ty -> do
      -- Not sure if this is correct....
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "typeRowEmpty"

    TypeError e ->
      absurd e

  genWrappedRow :: Partial => Wrapped (Row Void) -> Quine Void { lbls :: Array (Expr Void), mbTail :: Expr Void }
  genWrappedRow wrappedRow = do
    let
      { labels, tail } = view (_WrappedVals <<< _RowVal) wrappedRow
    lbls <- do
      ctor_Tuple <- liftCodegen $ importFrom "Data.Tuple" $ importCtor "Tuple" "Tuple"
      for labels \r -> do
        generatedType <- genType r.value
        pure $ exprApp ctor_Tuple
          [ exprString $ view (_NameVal <<< _Label) r.label
          , generatedType
          ]
    mbTail <- tail # genMaybe \ctor_Just ty -> do
      generatedType <- genType ty
      pure $ exprApp ctor_Just
        [ generatedType ]
    pure { lbls, mbTail }

  genFunDep :: Partial => ClassFundep -> Quine Void (Expr Void)
  genFunDep _ = do
    -- TODO: add import handling logic for qualified name
    pure $ exprString "genFunDep Not yet implemented"

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

  -- { head :: InstanceHead e
  -- , body :: Maybe (Tuple SourceToken (NonEmptyArray (InstanceBinding e)))
  -- }
  genDeclInstances :: Partial => Array (Instance Void) -> Quine Void (Array (Expr Void))
  genDeclInstances instances = do
    cg <- liftCodegen $ importFrom "Tidy.Codegen"
      { declInstance: importValue "declInstance"
      }
    -- declInstance mbName mbConstraints className tyCtors [ instValues ]
    for instances \inst -> do
      let { head, body } = view _InstanceVal inst
      mbName <- head.name # genMaybe \ctor_Just val -> do
        pure $ exprApp ctor_Just
          [ exprString $ viewOn val (_NameVal <<< _Ident) ]
      generatedConstraints <- traverse genType head.constraints
      generatedTypes <- traverse genType head.types
      generatedInstances <- genInstVals body
      pure $ exprApp cg.declInstance
        [ mbName
        , exprArray generatedConstraints
        , exprString $ viewOn head.className (_QualifiedNameVal (view _Proper))
        , exprArray generatedTypes
        , exprArray generatedInstances
        ]

  genInstVals :: Partial => Array (InstanceBinding Void) -> Quine Void (Array (Expr Void))
  genInstVals instArrs = do
    for instArrs case _ of
      -- InstanceBindingSignature (Labeled (Name Ident) (Type e))
      InstanceBindingSignature lbld -> do
        -- instSignature "name" ty
        let { label, value } = view _LabeledVals lbld
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { instSignature: importValue "instSignature"
          }
        generatedType <- genType value
        pure $ exprApp cg.instSignature
          [ exprString $ viewOn label (_NameVal <<< _Ident)
          , generatedType
          ]

      -- InstanceBindingName (ValueBindingFields e)
      InstanceBindingName { name, binders, guarded } -> do
        -- instValue name [ binderVar "a" ] $ exprString "foo"
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { instValue: importValue "instValue"
          }
        generatedBinders <- traverse genBinder binders
        generatedGuard <- genGuarded guarded
        pure $ exprApp cg.instValue
          [ exprString $ viewOn name (_NameVal <<< _Ident)
          , exprArray generatedBinders
          , generatedGuard
          ]

  genBinder :: Partial => Binder Void -> Quine Void (Expr Void)
  genBinder = case _ of
    -- BinderWildcard SourceToken
    BinderWildcard _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "binderWildcard"

    -- BinderVar (Name Ident)
    BinderVar varName -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderVar: importValue "binderVar"
        }
      pure $ exprApp cg.binderVar
        [ exprString $ viewOn varName (_NameVal <<< _Ident) ]

    -- BinderNamed (Name Ident) SourceToken (Binder e)
    BinderNamed name _ binder -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderNamed: importValue "binderNamed"
        }
      generatedBinder <- genBinder binder
      pure $ exprApp cg.binderNamed
        [ exprString $ viewOn name (_NameVal <<< _Ident)
        , generatedBinder
        ]

    -- BinderConstructor (QualifiedName Proper) (Array (Binder e))
    BinderConstructor ctor binders -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderCtor: importValue "binderCtor"
        }
      -- TODO: handle import and var name binding
      ctorName <- doImportCtor "Some.Module" "Type" (viewOn ctor (_QualifiedNameVal (view _Proper))) "varName"
      generatedBinders <- traverse genBinder binders
      pure $ exprApp cg.binderCtor
        [ ctorName
        , exprArray generatedBinders
        ]

    -- BinderBoolean SourceToken Boolean
    BinderBoolean _ bool -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderBool: importValue "binderBool"
        }
      pure $ exprApp cg.binderBool
        [ exprBool bool ]

    -- BinderChar SourceToken Char
    BinderChar _ char -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderChar: importValue "binderChar"
        }
      pure $ exprApp cg.binderChar
        [ exprChar char ]

    -- BinderString SourceToken String
    BinderString _ str -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderString: importValue "binderString"
        }
      pure $ exprApp cg.binderString
        [ exprString str ]

    -- BinderInt (Maybe SourceToken) SourceToken IntValue
    BinderInt negSign _ iValue -> do
      case iValue of
        SmallInt i -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { binderInt: importValue "binderInt"
            }
          pure $ exprApp cg.binderInt
            [ exprInt $ if isJust negSign then negate i else i ]
        BigInt s -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { binderInt: importValue "binderInt"
            }
          let i = fromJust $ Int.fromString s
          pure $ exprApp cg.binderInt
            [ exprInt $ if isJust negSign then negate i else i ]
        BigHex s -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { binderInt: importValue "binderInt"
            }
          let i = fromJust $ Int.fromString s
          pure $ exprApp cg.binderInt
            [ exprIntHex $ if isJust negSign then negate i else i ]

    -- BinderNumber (Maybe SourceToken) SourceToken Number
    BinderNumber negSign _ n -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderNumber: importValue "binderNumber"
        }
      pure $ exprApp cg.binderNumber
        [ exprNumber $ if isJust negSign then negate n else n ]

    -- BinderArray (Delimited (Binder e))
    BinderArray delBinders -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderArray: importValue "binderArray"
        }
      generatedBinders <- traverse genBinder $ toArrayOfOn delBinders (_DelimitedVals <<< folded)
      pure $ exprApp cg.binderArray
        [ exprArray generatedBinders ]

    -- BinderRecord (Delimited (RecordLabeled (Binder e)))
    BinderRecord delRcdBinders -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderRecord: importValue "binderRecord"
        }
      generatedBinders <- traverse genRecordLabeledBinder $ toArrayOfOn delRcdBinders (_DelimitedVals <<< folded)
      pure $ exprApp cg.binderRecord
        [ exprArray generatedBinders ]

    -- BinderParens (Wrapped (Binder e))
    BinderParens wrappedBinder -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderParens: importValue "binderParens"
        }
      generatedBinder <- genBinder $ viewOn wrappedBinder _WrappedVals
      pure $ exprApp cg.binderParens
        [ generatedBinder ]

    -- BinderTyped (Binder e) SourceToken (Type e)
    BinderTyped binder _ ty -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderTyped: importValue "binderTyped"
        }
      generatedBinder <- genBinder binder
      generatedType <- genType ty
      pure $ exprApp cg.binderTyped
        [ generatedBinder
        , generatedType
        ]

    -- BinderOp (Binder e) (NonEmptyArray (Tuple (QualifiedName Operator) (Binder e)))
    BinderOp binder binderOps -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderOp: importValue "binderOp"
        , binaryOp: importValue "binaryOp"
        }
      generatedBinder <- genBinder binder
      generatedBinderOps <- for binderOps \(Tuple qualOp b) -> do
        -- TODO: add import handling logic for qualified name
        generatedB <- genBinder b
        pure $ exprApp cg.binaryOp
          [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator))
          , generatedB
          ]
      pure $ exprApp cg.binderOp
        [ generatedBinder
        , exprArray $ NEA.toArray generatedBinderOps
        ]

    BinderError e ->
      absurd e

  genRecordLabeledBinder :: Partial => RecordLabeled (Binder Void) -> Quine Void (Expr Void)
  genRecordLabeledBinder = case _ of
    -- RecordPun (Name Ident)
    RecordPun varName -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderVar: importValue "binderVar"
        }
      -- TODO: ask Nate if this is correct
      pure $ exprApp cg.binderVar
        [ exprString $ viewOn varName (_NameVal <<< _Ident) ]

    -- RecordField (Name Label) SourceToken a
    RecordField varName _ binder -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { binderNamed: importValue "binderNamed"
        }
      generatedBinder <- genBinder binder
      -- TODO: ask Nate if this is correct
      pure $ exprApp cg.binderNamed
        [ exprString $ viewOn varName (_NameVal <<< _Label)
        , generatedBinder
        ]

  genRecordLabeledExpr :: Partial => RecordLabeled (Expr Void) -> Quine Void (Expr Void)
  genRecordLabeledExpr = case _ of
    -- RecordPun (Name Ident)
    RecordPun varName -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprIdent: importValue "exprIdent"
        }
      -- TODO: ask Nate if this is correct
      pure $ exprApp cg.exprIdent
        [ exprString $ viewOn varName (_NameVal <<< _Ident) ]

    -- RecordField (Name Label) SourceToken a
    RecordField varName _ expr -> do
      tuple <- liftCodegen $ importFrom "Data.Tuple"
        { ctor: importCtor "Tuple" "Tuple"
        }
      generatedExpr <- genExpr expr
      pure $ exprApp tuple.ctor
        [ exprString $ viewOn varName (_NameVal <<< _Label)
        , generatedExpr
        ]

  genGuarded :: Partial => Guarded Void -> Quine Void (Expr Void)
  genGuarded = case _ of
    Unconditional _ wher -> do
      genWhere wher
    Guarded guardedExprArr -> do
      genGuardedExprs guardedExprArr

  genWhere :: Partial => Where Void -> Quine Void (Expr Void)
  genWhere wher = do
    let { expr, bindings } = viewOn wher _WhereVal
    generatedExpr <- genExpr expr
    if Array.null bindings then do
      -- minor optimization. Outputs `someExpr`
      -- rather than `exprWhere (someExpr) []`.
      pure generatedExpr
    else do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprWhere: importValue "exprWhere"
        }
      generatedLetBinds <- genLetBindings bindings
      pure $ exprApp cg.exprWhere
        [ generatedExpr
        , exprArray generatedLetBinds
        ]

  genLetBindings :: Partial => Array (LetBinding Void) -> Quine Void (Array (Expr Void))
  genLetBindings binds = do
    for binds case _ of
      -- LetBindingSignature (Labeled (Name Ident) (Type e))
      LetBindingSignature lbld -> do
        let { label, value } = viewOn lbld _LabeledVals
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { letSignature: importValue "letSignature"
          }
        generatedType <- genType value
        pure $ exprApp cg.letSignature
          [ exprString $ viewOn label (_NameVal <<< _Ident)
          , generatedType
          ]

      -- LetBindingName (ValueBindingFields e)
      LetBindingName { name, binders, guarded } -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { letValue: importValue "letValue"
          }
        generatedBinders <- traverse genBinder binders
        generatedGuard <- genGuarded guarded
        pure $ exprApp cg.letValue
          [ exprString $ viewOn name (_NameVal <<< _Ident)
          , exprArray generatedBinders
          , generatedGuard
          ]

      -- LetBindingPattern (Binder e) SourceToken (Where e)
      LetBindingPattern binder _ wher -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { letBinder: importValue "letBinder"
          }
        generatedBinder <- genBinder binder
        generatedWhere <- genWhere wher
        pure $ exprApp cg.letBinder
          [ generatedBinder
          , generatedWhere
          ]

      LetBindingError e ->
        absurd e

  genGuardedExprs :: Partial => NonEmptyArray (GuardedExpr Void) -> Quine Void (Expr Void)
  genGuardedExprs geArr = do
    cg <- liftCodegen $ importFrom "Tidy.Codegen"
      { guardBranch: importValue "guardBranch"
      }
    guards <- traverse (genGuardedExpr cg) geArr
    pure $ exprArray $ NEA.toArray guards

  genGuardedExpr :: forall r. Partial => { guardBranch :: Expr Void | r } -> GuardedExpr Void -> Quine Void (Expr Void)
  genGuardedExpr cg guardedExpr = do
    let r = viewOn guardedExpr _GuardedExprVal
    generatedPatterns <- traverse genPatternGuard r.patterns
    generatedWhere <- genWhere r.where
    pure $ exprApp cg.guardBranch
      [ exprArray generatedPatterns
      , generatedWhere
      ]

  genPatternGuard :: Partial => PatternGuard Void -> Quine Void (Expr Void)
  genPatternGuard pGuard = do
    let { binder, expr } = viewOn pGuard _PatternGuardVal
    generatedExpr <- genExpr expr
    case binder of
      Nothing -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { guardExpr: importValue "guardExpr"
          }
        pure $ exprApp cg.guardExpr
          [ generatedExpr
          ]
      Just b -> do
        cg <- liftCodegen $ importFrom "Tidy.Codegen"
          { guardBinder: importValue "guardBinder"
          }
        generatedBinder <- genBinder b
        pure $ exprApp cg.guardBinder
          [ generatedBinder
          , generatedExpr
          ]

  genExpr :: Partial => Expr Void -> Quine Void (Expr Void)
  genExpr = case _ of
    -- ExprHole (Name Ident)
    ExprHole holeName -> do
      pure $ exprString "Tidy codegen not support expr hole"

    -- ExprSection SourceToken
    ExprSection _ -> do
      liftCodegen $ importFrom "Tidy.Codegen" $ importValue "exprSection"

    -- ExprIdent (QualifiedName Ident)
    ExprIdent qualIdent -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprIdent: importValue "exprIdent"
        }
      -- TODO: add import handling logic for qualified name
      pure $ exprApp cg.exprIdent
        [ exprString $ viewOn qualIdent (_QualifiedNameVal (view _Ident)) ]

    -- ExprConstructor (QualifiedName Proper)
    ExprConstructor qualProp -> do
      -- TODO: handle import logic here
      doImportCtor "Module" "Type" (viewOn qualProp (_QualifiedNameVal (view _Proper))) "varName"

    -- ExprBoolean SourceToken Boolean
    ExprBoolean _ bool -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprBool: importValue "exprBool"
        }
      pure $ exprApp cg.exprBool
        [ exprBool bool ]

    -- ExprChar SourceToken Char
    ExprChar _ char -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprChar: importValue "exprChar"
        }
      pure $ exprApp cg.exprChar
        [ exprChar char ]

    -- ExprString SourceToken String
    ExprString _ str -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprString: importValue "exprString"
        }
      pure $ exprApp cg.exprString
        [ exprString str ]

    -- ExprInt SourceToken IntValue
    ExprInt _ iValue -> do
      case iValue of
        SmallInt i -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { exprInt: importValue "exprInt"
            }
          pure $ exprApp cg.exprInt
            [ exprInt i ]
        BigInt s -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { exprInt: importValue "exprInt"
            }
          let i = fromJust $ Int.fromString s
          pure $ exprApp cg.exprInt
            [ exprInt i ]
        BigHex s -> do
          cg <- liftCodegen $ importFrom "Tidy.Codegen"
            { exprIntHex: importValue "exprIntHex"
            }
          let i = fromJust $ Int.fromString s
          pure $ exprApp cg.exprIntHex
            [ exprIntHex i ]

    -- ExprNumber SourceToken Number
    ExprNumber _ n -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprNumber: importValue "exprNumber"
        }
      pure $ exprApp cg.exprNumber
        [ exprNumber n ]

    -- ExprArray (Delimited (Expr e))
    ExprArray delExprs -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprArray: importValue "exprArray"
        }
      generatedExprs <- traverse genExpr $ toArrayOfOn delExprs (_DelimitedVals <<< folded)
      pure $ exprApp cg.exprArray
        [ exprArray generatedExprs ]

    -- ExprRecord (Delimited (RecordLabeled (Expr e)))
    ExprRecord delRcdExprs -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprRecord: importValue "exprRecord"
        }
      generatedFields <- traverse genRecordLabeledExpr $ toArrayOfOn delRcdExprs (_DelimitedVals <<< folded)
      pure $ exprApp cg.exprRecord
        [ exprArray generatedFields ]

    -- ExprParens (Wrapped (Expr e))
    ExprParens wrappedExpr -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprParens: importValue "exprParens"
        }
      generatedExpr <- genExpr $ viewOn wrappedExpr _WrappedVals
      pure $ exprApp cg.exprParens
        [ generatedExpr ]

    -- ExprTyped (Expr e) SourceToken (Type e)
    ExprTyped expr _ ty -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprTyped: importValue "exprTyped"
        }
      generatedExpr <- genExpr expr
      generatedType <- genType ty
      pure $ exprApp cg.exprTyped
        [ generatedExpr
        , generatedType
        ]

    -- ExprInfix (Expr e) (NonEmptyArray (Tuple (Wrapped (Expr e)) (Expr e)))
    ExprInfix expr infixExprs -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprInfix: importValue "exprInfix"
        }
      generatedExpr <- genExpr expr
      generatedInfixExprs <- do
        tuple <- liftCodegen $ importFrom "Data.Tuple"
          { ctor: importCtor "Tuple" "Tuple"
          }
        for infixExprs \(Tuple wrappedExpr nextExpr) -> do
          leftExpr <- genExpr $ viewOn wrappedExpr _WrappedVals
          rightExpr <- genExpr nextExpr
          pure $ exprApp tuple.ctor
            [ leftExpr
            , rightExpr
            ]
      pure $ exprApp cg.exprInfix
        [ generatedExpr
        , exprArray $ NEA.toArray generatedInfixExprs
        ]

    -- ExprOp (Expr e) (NonEmptyArray (Tuple (QualifiedName Operator) (Expr e)))
    ExprOp expr binOps -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprOp: importValue "exprOp"
        , binaryOp: importValue "binaryOp"
        }
      generatedExpr <- genExpr expr
      generatedInfixExprs <- for binOps \(Tuple qualOp nextExpr) -> do
        -- TODO: add import handling logic for qualified name
        generatedNextExpr <- genExpr nextExpr
        pure $ exprApp cg.binaryOp
          [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator))
          , generatedNextExpr
          ]
      pure $ exprApp cg.exprOp
        [ generatedExpr
        , exprArray $ NEA.toArray generatedInfixExprs
        ]

    -- ExprOpName (QualifiedName Operator)
    ExprOpName qualOp -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprOpName: importValue "exprOpName"
        }
      -- TODO: add import handling logic for qualified name
      pure $ exprApp cg.exprOpName
        [ exprString $ viewOn qualOp (_QualifiedNameVal (view _Operator)) ]

    -- ExprNegate SourceToken (Expr e)
    ExprNegate _ expr -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprNegate: importValue "exprNegate"
        }
      generatedExpr <- genExpr expr
      pure $ exprApp cg.exprNegate
        [ generatedExpr ]

    -- ExprRecordAccessor (RecordAccessor e)
    ExprRecordAccessor { expr, path } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprDot: importValue "exprDot"
        }
      generatedExpr <- genExpr expr
      let generatedPath = toArrayOfOn path (_SeparatedVals <<< folded <<< _NameVal <<< _Label <<< to exprString)
      pure $ exprApp cg.exprDot
        [ generatedExpr
        , exprArray generatedPath
        ]

    -- ExprRecordUpdate (Expr e) (DelimitedNonEmpty (RecordUpdate e))
    ExprRecordUpdate expr delNeRcdUpd -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprUpdate: importValue "exprUpdate"
        }
      generatedExpr <- genExpr expr
      generatedUpdates <- traverse genRecordUpdate $ toArrayOfOn delNeRcdUpd (_DelimitedNonEmptyVals <<< folded)
      pure $ exprApp cg.exprUpdate
        [ generatedExpr
        , exprArray generatedUpdates
        ]

    -- ExprApp (Expr e) (NonEmptyArray (Expr e))
    ExprApp f args -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprApp: importValue "exprApp"
        }
      generatedFn <- genExpr f
      generatedArgs <- traverse genExpr args
      pure $ exprApp cg.exprApp
        [ generatedFn
        , exprArray $ NEA.toArray generatedArgs
        ]

    -- ExprLambda (Lambda e)
    ExprLambda { binders, body } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprLambda: importValue "exprLambda"
        }
      generatedBinders <- traverse genBinder binders
      generatedBody <- genExpr body
      pure $ exprApp cg.exprLambda
        [ exprArray $ NEA.toArray generatedBinders
        , generatedBody
        ]

    -- ExprIf (IfThenElse e)
    ExprIf ite -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprIf: importValue "exprIf"
        }
      generatedCond <- genExpr ite.cond
      generatedTrue <- genExpr ite.true
      generatedFalse <- genExpr ite.false
      pure $ exprApp cg.exprIf
        [ generatedCond
        , generatedTrue
        , generatedFalse
        ]

    -- ExprCase (CaseOf e)
    ExprCase { head, branches } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprCase: importValue "exprCase"
        , caseBranch: importValue "caseBranch"
        , guardBranch: importValue "guardBranch"
        }
      generatedHead <- traverse genExpr $ toArrayOfOn head (_SeparatedVals <<< folded)
      generatedBranches <- for branches \(Tuple sepBinders guardedExpr) -> do
        generatedBinders <- traverse genBinder $ toArrayOfOn sepBinders (_SeparatedVals <<< folded)
        generatedGuard <- genGuarded guardedExpr
        pure $ exprApp cg.caseBranch
          [ exprArray generatedBinders
          , generatedGuard
          ]
      pure $ exprApp cg.exprCase
        [ exprArray generatedHead
        , exprArray $ NEA.toArray generatedBranches
        ]

    -- ExprLet (LetIn e)
    ExprLet { bindings, body } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprLet: importValue "exprLet"
        , letBinder: importValue "letBinder"
        }
      generatedBindings <- genLetBindings $ NEA.toArray bindings
      generatedBody <- genExpr body
      pure $ exprApp cg.exprLet
        [ exprArray generatedBindings
        , generatedBody
        ]

    -- ExprDo (DoBlock e)
    ExprDo { statements } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprDo: importValue "exprDo"
        }
      generatedStatements <- traverse genDoStatement statements
      let { init, last } = NEA.unsnoc generatedStatements
      pure $ exprApp cg.exprDo
        [ exprArray init
        , last
        ]

    -- ExprAdo (AdoBlock e)
    ExprAdo { statements, result } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { exprAdo: importValue "exprAdo"
        }
      generatedStatements <- traverse genDoStatement statements
      generatedResult <- genExpr result
      pure $ exprApp cg.exprAdo
        [ exprArray generatedStatements
        , generatedResult
        ]

    ExprError e ->
      absurd e

  genRecordUpdate :: Partial => RecordUpdate Void -> Quine Void (Expr Void)
  genRecordUpdate = case _ of
    -- RecordUpdateLeaf (Name Label) SourceToken (Expr e)
    RecordUpdateLeaf lblName _ expr -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { update: importValue "update"
        }
      generatedExpr <- genExpr expr
      pure $ exprApp cg.update
        [ exprString $ viewOn lblName (_NameVal <<< _Label)
        , generatedExpr
        ]

    -- RecordUpdateBranch (Name Label) (DelimitedNonEmpty (RecordUpdate e))
    RecordUpdateBranch lblName nestedUpdates -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { updateNested: importValue "updateNested"
        }
      generatedUpdates <- traverse genRecordUpdate $ toArrayOfOn nestedUpdates (_DelimitedNonEmptyVals <<< folded)
      pure $ exprApp cg.updateNested
        [ exprString $ viewOn lblName (_NameVal <<< _Label)
        , exprArray generatedUpdates
        ]

  genDoStatement :: Partial => DoStatement Void -> Quine Void (Expr Void)
  genDoStatement = case _ of
    -- DoLet SourceToken (NonEmptyArray (LetBinding e))
    DoLet _ letBindings -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { doLet: importValue "doLet"
        }
      generatedLetBinds <- genLetBindings $ NEA.toArray letBindings
      pure $ exprApp cg.doLet
        [ exprArray generatedLetBinds
        ]

    -- DoDiscard (Expr e)
    DoDiscard expr -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { doDiscard: importValue "doDiscard"
        }
      generatedExpr <- genExpr expr
      pure $ exprApp cg.doDiscard
        [ generatedExpr
        ]

    -- DoBind (Binder e) SourceToken (Expr e)
    DoBind binder _ expr -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { doBind: importValue "doBind"
        }
      generatedBinder <- genBinder binder
      generatedExpr <- genExpr expr
      pure $ exprApp cg.doBind
        [ generatedBinder
        , generatedExpr
        ]

    DoError e ->
      absurd e

genMaybe :: forall a. Partial => (Expr Void -> a -> Quine Void (Expr Void)) -> Maybe a -> Quine Void (Expr Void)
genMaybe f = case _ of
  Nothing -> do
    liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Nothing"
  Just val -> do
    ctor_Just <- liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Just"
    f ctor_Just val
