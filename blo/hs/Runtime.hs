module Runtime
    (RuntimeAst(..),RuntimeType(..),RuntimeFunc(..),
     Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSize,astTypeImportSize,
     astTypeErrorName,astTypeSourcePos,astTypeIsImport,
     annotateRuntime)
where

import Compile(Compile,SourcePos,compileError)
import Check
    (Ast(..),AstType(..),AstFunc(..),AstFuncSig(..),AstStmt,
     astTypeName,astTypeSize,astTypeImportSize,
     astTypeErrorName,astTypeSourcePos,astTypeIsImport,
     astTypeImportSize)

class RuntimeType rtt where
    annotateType :: AstType -> Compile rtt

class RuntimeFunc rtf where
    annotateFunc :: AstFuncSig -> Compile rtf

data RuntimeAst rtt rtf =
    RuntimeAst [(AstType,Maybe rtt)] [(AstFuncSig,Either AstStmt rtf)]

annotateRuntime :: (RuntimeType rtt, RuntimeFunc rtf) =>
        Ast -> Compile (RuntimeAst rtt rtf)
annotateRuntime (Ast astTypes astFuncs) = do
    types <- mapM annotateAstType astTypes
    funcs <- mapM annotateAstFunc astFuncs
    return (RuntimeAst types funcs)
  where
    annotateAstType astType
      | astTypeIsImport astType = do
            rtt <- annotateType astType
            return (astType,Just rtt)
      | otherwise = return (astType,Nothing)
    annotateAstFunc (AstImportFunc astFuncSig) = do
        rtf <- annotateFunc astFuncSig
        return (astFuncSig,Right rtf)
    annotateAstFunc (AstFunc astFuncSig astStmt) =
        return (astFuncSig,Left astStmt)
