module Ast
where

import Compile(SourcePos)

data Identifier = Identifier SourcePos String

data PartialDef = PartialDef Identifier [Identifier] [Token] SourcePos
data Token = Token SourcePos String

data Def = Def Identifier Int Expr
data Expr =
    ExprBag SourcePos [(Int,Expr)]
  | ExprBound SourcePos String Int
  | ExprFuncall SourcePos Def [(Bool,Expr)]
  | ExprUnion SourcePos Expr Expr
  | ExprIntersect SourcePos Expr Expr
  | ExprDiff SourcePos Expr Expr
