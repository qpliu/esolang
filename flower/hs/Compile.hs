module Compile
    (Defn(Defn),Stmt(StmtSend,StmtIf,StmtBind),
     Expr(ExprLiteral,ExprBound,ExprCall),IfBlk(IfBlk),
     compile)
where

-- Transform the syntax tree from Parse by converting all references by name
-- to indices or direct references.

import Data.List(elemIndex)
import Data.Map(Map,fromAscList)
import Data.Maybe(catMaybes)
import qualified Data.Map

import Parse
    (Definition(Definition),
     Statement(IfStatement,SendStatement,BindStatement),
     IfBlock(IfBlock),
     Expression(ExpressionLiteral,ExpressionBound,ExpressionCall),
     Symbol(Symbol),
     Literal(Literal))

data Defn = Defn Int Int [Stmt] -- input count, output count
    deriving Show

data Stmt = StmtSend [Expr] [Int] -- [output index]
          | StmtIf [IfBlk]
          | StmtBind [Expr] -- prepends to locals
    deriving Show

data Expr = ExprLiteral Bool
          | ExprBound Int -- index into locals (as it currently is, given that BindStmt changes indices of existing bindings)
          | ExprCall Defn [Expr]
    deriving Show

data IfBlk = IfBlk [Expr] [Stmt]
    deriving Show

compile :: [Definition] -> String -> Maybe Defn
compile definitions = flip Data.Map.lookup (compileDefs definitions)

compileDefs :: [Definition] -> Map String Defn
compileDefs definitions = defs
  where
    defs = fromAscList (map (compileDef defs) definitions)

compileDef :: Map String Defn -> Definition -> (String,Defn)
compileDef defs (Definition _ (Symbol _ name) inputs outputs statements) =
    (name,Defn (length inputs) (length outputs) (compileStmts defs inputs outputs statements))

compileStmts :: Map String Defn -> [Symbol] -> [Symbol] -> [Statement] -> [Stmt]
compileStmts defs locals outputs statements =
    (reverse . snd . foldl (compileStmt defs outputs) (locals,[])) statements

compileStmt :: Map String Defn -> [Symbol] -> ([Symbol],[Stmt]) -> Statement -> ([Symbol],[Stmt])
compileStmt defs outputs (locals,stmts) statement =
    case statement of
        IfStatement _ ifBlocks -> (locals,StmtIf (map (compileIfBlk defs locals outputs) ifBlocks):stmts)
        SendStatement _ _ expressions symbols -> (locals,StmtSend (map (compileExpr defs locals) expressions) ((catMaybes . map (`elemIndex` outputs)) symbols):stmts)
        BindStatement _ symbols expressions -> (symbols ++ locals,StmtBind (map (compileExpr defs locals) expressions):stmts)
        _ -> error ("compileStmt:" ++ show statement)

compileIfBlk :: Map String Defn -> [Symbol] -> [Symbol] -> IfBlock -> IfBlk
compileIfBlk defs locals outputs (IfBlock _ expressions statements) =
    IfBlk (map (compileExpr defs locals) expressions) (compileStmts defs locals outputs statements)

compileExpr :: Map String Defn -> [Symbol] -> Expression -> Expr
compileExpr defs locals expression =
    case expression of
        ExpressionLiteral (Literal _ bit) -> ExprLiteral bit
        ExpressionBound symbol ->
            maybe (error ("compileExpr:" ++ show expression)) ExprBound (elemIndex symbol locals)
        ExpressionCall (Symbol _ name) expressions _ ->
            maybe (error ("compileExpr:" ++ show expression)) (flip ExprCall (map (compileExpr defs locals) expressions)) (Data.Map.lookup name defs)
        _ -> error ("compileExpr:" ++ show expression)
