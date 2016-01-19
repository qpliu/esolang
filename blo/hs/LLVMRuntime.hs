module LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..))
where

import LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,
     writeCode,writeRefCountType,writeOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),Compile,SourcePos,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSourcePos,astTypeIsImport,astTypeErrorName,astTypeSize)

data LLVMRuntimeType = LLVMRuntimeType [CodeGen ()]
data LLVMRuntimeFunc = LLVMRuntimeFunc [CodeGen ()] (CodeGen ())

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
