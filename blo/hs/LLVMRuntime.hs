module LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..),
     importTypeDeclarations,importFuncDeclarations,importFuncCode)
where

import Runtime
    (RuntimeType(..),RuntimeFunc(..),Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSourcePos,astTypeIsImport,astTypeErrorName,astTypeSize)

data LLVMRuntimeType = LLVMRuntimeType [String]
data LLVMRuntimeFunc = LLVMRuntimeFunc [String] String

importTypeDeclarations :: LLVMRuntimeType -> [String]
importTypeDeclarations (LLVMRuntimeType decls) = decls

importFuncDeclarations :: LLVMRuntimeFunc -> [String]
importFuncDeclarations (LLVMRuntimeFunc decls _) = decls

importFuncCode :: LLVMRuntimeFunc -> String
importFuncCode (LLVMRuntimeFunc _ code) = code

instance RuntimeType LLVMRuntimeType where
    annotateType astType
      | astTypeName astType == "stack" && astTypeIsImport astType =
            compileError (astTypeSourcePos astType)
                         ("Unknown import " ++ astTypeErrorName astType)
    annotateType astType =
        compileError (astTypeSourcePos astType)
                     ("Unknown import " ++ astTypeErrorName astType)

instance RuntimeFunc LLVMRuntimeFunc where
    annotateFunc (AstFuncSig pos name@"getByte" [_] Nothing) =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"putByte" [_] Nothing) =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"pushStack" [(_,stack),(_,bit)] Nothing)
      | astTypeName stack == "stack" && astTypeIsImport stack =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"popStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"isEmptyStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")
