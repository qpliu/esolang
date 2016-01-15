module Interp(callFunc)
where

import LowLevel
    (Type,Func(..),FuncSig(..),Stmt(..),Expr(..),
     stmtLeavingScope,stmtNext)
import Memory(Memory)
import Value
    (Value,Data,
     addVar,reassignVar,copyValue,removeVars,valueBit,valueField,
     refValue,unrefValue)
import InterpRuntime
    (InterpRuntimeType,InterpRuntimeFunc(..),InterpRuntimeValue,
     newRuntimeValue)

type Mem = Memory (Data InterpRuntimeValue)

callFunc :: Mem -> [Value] -> Func InterpRuntimeType InterpRuntimeFunc
                -> IO (Mem, Maybe Value)
callFunc mem args (ImportFunc _ (InterpRuntimeFunc func)) = func mem args
callFunc mem args (Func (FuncSig argTypes retType) stmt) = do
    let scope = zipWith (\ value (name,_) -> (name,value)) args argTypes
    execStmt mem scope stmt

execStmt :: Mem -> [(String,Value)] -> Stmt InterpRuntimeType InterpRuntimeFunc
                -> IO (Mem, Maybe Value)
execStmt mem scope = exec
  where
    finishStmt newMem newScope stmt nextStmt =
        let (nextScope,nextMem) =
                removeVars (stmtLeavingScope stmt) newScope newMem
        in  maybe (return (nextMem,Nothing)) (execStmt nextMem nextScope)
                  nextStmt
    exec stmt@(StmtBlock _ _) = finishStmt mem scope stmt (stmtNext stmt)
    exec stmt@(StmtVar _ name varType expr) = undefined
    exec stmt@(StmtIf _ expr ifBlock elseBlock) = undefined
    exec stmt@(StmtFor _ forBlock) = finishStmt mem scope stmt (Just forBlock)
    exec stmt@(StmtBreak _) = finishStmt mem scope stmt (stmtNext stmt)
    exec stmt@(StmtReturn _ Nothing) = finishStmt mem scope stmt Nothing
    exec stmt@(StmtReturn _ expr) = undefined
    exec stmt@(StmtSetClear _ bit expr) = undefined
    exec stmt@(StmtAssign _ lhs rhs) = undefined
    exec stmt@(StmtExpr _ expr) = undefined
