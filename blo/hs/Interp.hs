module Interp(callFunc)
where

import Control.Monad(foldM)

import LowLevel
    (Type,Func(..),FuncSig(..),Stmt(..),Expr(..),
     stmtLeavingScope,stmtNext)
import Memory(Memory)
import Value
    (Value,Data,
     newValue,reassignVar,copyValue,removeVars,valueBit,valueSetBit,valueField,
     refValue,unrefValue)
import InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc(..),InterpRuntimeValue,
     newRuntimeValue)

type Mem = Memory (Data InterpRuntimeValue)

callFunc :: Mem -> [Value] -> Func InterpRuntimeType InterpRuntimeFunc
                -> IO (Mem, Maybe Value)
callFunc mem args (ImportFunc (FuncSig _ retType) (InterpRuntimeFunc func)) =
    func (fmap (newValue newRuntimeValue) retType) mem args
callFunc mem args (Func (FuncSig argTypes retType) stmt) = do
    let scope = zipWith (\ value (name,_) -> (name,value)) args argTypes
    execStmt mem scope stmt

execStmt :: Mem -> [(String,Value)] -> Stmt InterpRuntimeType InterpRuntimeFunc
                -> IO (Mem, Maybe Value)
execStmt mem scope = exec
  where
    finishStmt newMem newScope stmt nextStmt =
        let (nextScope,nextMem) =
                removeVars (stmtLeavingScope stmt nextStmt) newScope newMem
        in  maybe (return (nextMem,Nothing)) (execStmt nextMem nextScope)
                  nextStmt
    exec stmt@(StmtBlock _ _) = finishStmt mem scope stmt (stmtNext stmt)
    exec stmt@(StmtVar _ name varType _ Nothing) =
        let (val,mem1) = newValue newRuntimeValue varType mem
        in  finishStmt mem1 ((name,val):scope) stmt (stmtNext stmt)
    exec stmt@(StmtVar _ name varType _ (Just expr)) = do
        (mem1,Just value) <- evalExpr mem scope expr
        finishStmt mem1 ((name,value):scope) stmt (stmtNext stmt)
    exec stmt@(StmtIf _ expr ifBlock elseBlock) = do
        (mem1,Just value) <- evalExpr mem scope expr
        let bit = valueBit value 0 mem1
        let mem2 = unrefValue value mem1
        if bit
            then finishStmt mem2 scope stmt (Just ifBlock)
            else finishStmt mem2 scope stmt (maybe (stmtNext stmt)
                                                   Just elseBlock)
    exec stmt@(StmtFor _ forBlock) = finishStmt mem scope stmt (Just forBlock)
    exec stmt@(StmtBreak _) = finishStmt mem scope stmt (stmtNext stmt)
    exec stmt@(StmtReturn _ Nothing) = finishStmt mem scope stmt Nothing
    exec stmt@(StmtReturn _ (Just expr)) = do
        (mem1,retValue) <- evalExpr mem scope expr
        let (_,mem2) = removeVars (stmtLeavingScope stmt Nothing) scope mem1
        return (mem2,retValue)
    exec stmt@(StmtSetClear _ bit expr) = do
        (mem1,Just value) <- evalExpr mem scope expr
        let mem2 = (unrefValue value . valueSetBit value 0 bit) mem1
        finishStmt mem2 scope stmt (stmtNext stmt)
    exec stmt@(StmtAssign _ (ExprVar lhs) rhs) = do
        (mem1,Just rvalue) <- evalExpr mem scope rhs
        let (scope2,mem2) = reassignVar lhs rvalue scope mem1
        finishStmt mem2 scope2 stmt (stmtNext stmt)
    exec stmt@(StmtAssign _ lhs rhs) = do
        (mem1,Just lvalue) <- evalExpr mem scope lhs
        (mem2,Just rvalue) <- evalExpr mem1 scope rhs
        let mem3 = (unrefValue lvalue .
                    unrefValue rvalue .
                    copyValue rvalue lvalue) mem2
        finishStmt mem3 scope stmt (stmtNext stmt)
    exec stmt@(StmtExpr _ expr) = do
        (mem1,value) <- evalExpr mem scope expr
        let mem2 = maybe mem1 (flip unrefValue mem1) value
        finishStmt mem2 scope stmt (stmtNext stmt)

evalExpr :: Mem -> [(String,Value)] -> Expr InterpRuntimeType InterpRuntimeFunc
                -> IO (Mem, Maybe Value)
evalExpr mem scope = eval
  where
    eval (ExprVar varName) =
        let Just value = lookup varName scope
        in  return (refValue value mem,Just value)
    eval (ExprFunc _ func exprs _) = do
        (newMem,args) <- foldM evalParam (mem,[]) exprs
        callFunc newMem (reverse args) func
      where
        evalParam (oldMem,args) expr = do
            (newMem,Just arg) <- evalExpr oldMem scope expr
            return (newMem,arg:args)
    eval (ExprField offset importOffset fieldType expr) = do
        (newMem,Just value) <- evalExpr mem scope expr
        return (newMem,Just (valueField value offset importOffset fieldType))
