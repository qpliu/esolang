module LLVMCodeGen
    (codeGen)
where

import qualified Data.Set as S

import LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..),declarations,importFuncCode)
import LowLevel
    (Type,Func(..),FuncSig(..),Stmt(..),Expr(..),
     stmtLeavingScope,stmtNext)

codeGen :: [(String,Func LLVMRuntimeType LLVMRuntimeFunc)] -> String
codeGen funcs =
    (concat . S.toList . S.fromList . concatMap (getDeclarations . snd)) funcs
        ++ concatMap codeGenFunc funcs

getDeclarations :: Func LLVMRuntimeType LLVMRuntimeFunc -> [String]
getDeclarations (Func _ _) = []
getDeclarations (ImportFunc _ rtf) = declarations rtf

codeGenFunc :: (String,Func LLVMRuntimeType LLVMRuntimeFunc) -> String
codeGenFunc (_,ImportFunc _ rtf) = importFuncCode rtf
codeGenFunc (name,Func (FuncSig params retType) stmt) = undefined
