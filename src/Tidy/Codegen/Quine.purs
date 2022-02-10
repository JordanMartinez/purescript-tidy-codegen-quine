module Tidy.Codegen.Quine where

import Prelude
import Prim hiding (Type, Row)

import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.Array.NonEmpty as NEA
import Data.Either (either)
import Data.Lens (_1, _Just, folded, preview, toArrayOf, toArrayOfOn, view, viewOn)
import Data.Lens.Lens.Tuple (_2)
import Data.Map as Map
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (unwrap)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..), snd)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import PureScript.CST.RecordLens (_value)
import PureScript.CST.Types (Binder, ClassFundep, DataCtor(..), Declaration(..), Expr, Fixity(..), FixityOp(..), Foreign(..), Guarded(..), Ident, Instance, InstanceBinding(..), Labeled, Module(..), ModuleBody(..), ModuleHeader(..), ModuleName(..), Name(..), Role(..), Row, Type(..), TypeVarBinding(..), Wrapped)
import PureScript.CST.Types.Lens (_Ident, _Label, _Operator, _Proper, _SourceToken, _TokLowerName)
import Tidy.Codegen (declSignature, declValue, exprApp, exprArray, exprDo, exprIdent, exprInt, exprOp, exprString, exprWhere, letValue, typeApp)
import Tidy.Codegen.Monad (codegenModule, importCtor, importFrom, importOp, importOpen, importType, importValue)
import Tidy.Codegen.Quine.LensUtils (_InstanceVal, _LabeledVals, _NameVal, _OneOrDelimitedVals, _QualifiedNameVal, _RowVal, _SeparatedVals, _WrappedVals)
import Tidy.Codegen.Quine.Monad (Quine, codegenQuine, doImportType, liftCodegen)

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
    DeclValue { name, binders, guarded } -> do
      cg <- liftCodegen $ importFrom "Tidy.Codegen"
        { declValue: importValue "declValue"
        }
      generatedBinders <- genBinders binders
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
        generatedBinders <- genBinders binders
        generatedGuard <- genGuarded guarded
        pure $ exprApp cg.instValue
          [ exprString $ viewOn name (_NameVal <<< _Ident)
          , exprArray generatedBinders
          , generatedGuard
          ]

  genBinders :: Partial => Array (Binder Void) -> Quine Void (Array (Expr Void))
  genBinders binders = do
    for binders \_ ->
      pure $ exprString "TODO"

  genGuarded :: Partial => Guarded Void -> Quine Void (Expr Void)
  genGuarded = case _ of
    Unconditional _ wher ->
      pure $ exprString "TODO"
    Guarded guardExprArr -> do
      pure $ exprString "TODO"


genMaybe :: forall a. Partial => (Expr Void -> a -> Quine Void (Expr Void)) -> Maybe a -> Quine Void (Expr Void)
genMaybe f = case _ of
  Nothing -> do
    liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Nothing"
  Just val -> do
    ctor_Just <- liftCodegen $ importFrom "Data.Maybe" $ importCtor "Maybe" "Just"
    f ctor_Just val