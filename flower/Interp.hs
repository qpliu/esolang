module Interp(eval,interp) where

import Data.List(sort,(!!))

import Compile
    (Defn(Defn),Stmt(StmtSend,StmtIf,StmtBind),
     Expr(ExprLiteral,ExprBound,ExprCall),IfBlk(IfBlk))

-- first version does not implement dataflow semantics

eval :: Defn -> [Bool] -> [Bool]
eval (Defn _ _ stmts) inputs = (map snd . sort . evalStmts inputs []) stmts

evalStmts :: [Bool] -> [(Int,Bool)] -> [Stmt] -> [(Int,Bool)]
evalStmts locals outputs stmts = (snd . foldl evalStmt (locals,outputs)) stmts

evalStmt :: ([Bool],[(Int,Bool)]) -> Stmt -> ([Bool],[(Int,Bool)])
evalStmt (locals,outputs) stmt =
    case stmt of
        StmtSend exprs indices -> (locals,zip indices (evalExprs locals exprs) ++ outputs)
        StmtIf ifBlks -> (locals,evalIfBlks locals outputs ifBlks)
        StmtBind exprs -> (evalExprs locals exprs ++ locals,outputs)

evalIfBlks :: [Bool] -> [(Int,Bool)] -> [IfBlk] -> [(Int,Bool)]
evalIfBlks locals outputs ifBlks =
    case ifBlks of
        [] -> outputs -- should not happen
        IfBlk [] stmts:_ -> evalStmts locals outputs stmts
        IfBlk exprs stmts:ifBlks ->
            case evalExprs locals exprs of
                [] -> evalIfBlks locals outputs ifBlks -- should not happen
                [False] -> evalIfBlks locals outputs ifBlks
                [True] -> evalStmts locals outputs stmts
                args ->
                    let argc2 = length args `div` 2
                    in  if take argc2 args == drop argc2 args
                            then evalStmts locals outputs stmts
                            else evalIfBlks locals outputs ifBlks

evalExprs :: [Bool] -> [Expr] -> [Bool]
evalExprs locals exprs = concatMap (evalExpr locals) exprs

evalExpr :: [Bool] -> Expr -> [Bool]
evalExpr locals expr =
    case expr of
        ExprLiteral bit -> [bit]
        ExprBound index -> [locals !! index]
        ExprCall defn exprs -> eval defn (evalExprs locals exprs)

interp :: Defn -> [Bool] -> [Bool]
interp defn@(Defn inputCount _ _) input =
    loop (eval defn (replicate inputCount False) ++ repeat False) input False False
  where
    loop (eof:readNext:writeBit:bit:state) input lastEof lastBit
      | eof = []
      | writeBit = bit : loop2
      | otherwise = loop2
      where
        loop2
          | not readNext = loop (eval defn (take inputCount (lastEof:lastBit:state ++ repeat False))) input lastEof lastBit
          | null input = loop (eval defn (take inputCount (True:lastBit:state ++ repeat False))) input True lastBit
          | otherwise = loop (eval defn (take inputCount (False:head input:state ++ repeat False))) (tail input) False (head input)
