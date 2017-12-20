module Ast
    (Module(Library,Program),
     Name,Var,Val,
     Use(Use),
     Export(Export),
     Subroutine(Subroutine),
     Statement(LetEq,LetAddEdge,LetRemoveEdge,If,
        Call,Return,DoLoop,DoEdges,Exit),
     IfBranch(IfEq,IfEdge),
     Location(Location))
where

data Module =
    Library Location Name [Use] [Subroutine] [Export]
  | Program Location Name [Use] [Subroutine] [Statement]
    deriving Show

type Name = String
type Var = String
type Val = String

data Use = Use Location String
    deriving Show

data Export = Export Location String
    deriving Show

data Subroutine = Subroutine Location Name [Var] [Statement]
    deriving Show

data Statement =
    LetEq Location Var Val
  | LetAddEdge Location Var Val
  | LetRemoveEdge Location Var Var
  | If [IfBranch] [Statement]
  | Call Location Name Name [Val]
  | Return Location
  | DoLoop Location Var [Statement]
  | DoEdges Location Var Var [Statement]
  | Exit Location Var
    deriving Show

data IfBranch =
    IfEq Location Var Var [Statement]
  | IfEdge Location Var Var [Statement]
    deriving Show

data Location = Location String Integer String
    deriving Show
