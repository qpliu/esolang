module Ast
    (Identifier(..),PartialDef(..),Param(..),Unparsed(..),
     Def(..),Expr(..),Func)
where

import Compile(SourcePos)

data Identifier = Identifier SourcePos String

data PartialDef = PartialDef Identifier [Param] [Unparsed]

data Param =
    ParamBound SourcePos [Bool] Identifier
  | ParamIgnored SourcePos [Bool]
  | ParamLiteral SourcePos [Bool]

data Unparsed =
    UnparsedIdentifier Identifier
  | UnparsedLiteral SourcePos [Bool]

type Func = (String,[Def])

data Def = Def [Param] Expr

data Expr =
    ExprLiteral SourcePos [Bool]
  | ExprBound Int
  | ExprFuncall Identifier [Expr]
  | ExprConcat Expr Expr
