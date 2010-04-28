module SyntaxTree(Bits,Pattern(Literal,Binding,Wildcard),Expr(LiteralBits,Concat,Bound,Call),Definition(Def)) where

type Bits = [Bool]
data Pattern = Literal Bits | Binding Bits String | Wildcard Bits
data Expr = LiteralBits Bits | Concat Expr Expr | Bound Int | Call String [Expr]
data Definition = Def [Pattern] Expr
