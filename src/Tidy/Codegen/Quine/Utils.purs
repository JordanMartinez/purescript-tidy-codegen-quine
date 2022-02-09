module Tidy.Codegen.Quine.Utils where

import Prim hiding (Type)

import PureScript.CST.Types (Expr)
import Tidy.Codegen (exprApp)

exprApp1 :: forall e. Expr e -> Expr e -> Expr e
exprApp1 ident arg = exprApp ident [ arg ]
