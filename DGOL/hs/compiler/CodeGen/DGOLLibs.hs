{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.DGOLLibs(
    dgolLibDecls,dgolLibDefs
)
where

import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand)
import LLVM.AST.Type(Type,void,i8,i32,ptr)
import LLVM.IRBuilder.Instruction(alloca,retVoid)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),extern,function)
import LLVM.IRBuilder.Monad(block)

import CodeGen.Runtime(newNode)
import CodeGen.Types(pFrameType)
import CodeGen.Util(
    intConst,eq,
    functionRef,
    call)

dgolLibDecls :: String -> Maybe (ModuleBuilder ())
dgolLibDecls name
  | name == "IO" = Just ioLibDecls
  | otherwise = Nothing

dgolLibDefs :: String -> Maybe (ModuleBuilder ())
dgolLibDefs name
  | name == "IO" = Just ioLibDefs
  | otherwise = Nothing

ioLibDecls :: ModuleBuilder ()
ioLibDecls = do
    ioLibReadbyteDecl
    ioLibWritebyteDecl
    return ()

ioLibDefs :: ModuleBuilder ()
ioLibDefs = do
    readDecl
    writeDecl
    ioLibReadbyteImpl
    ioLibWritebyteImpl
    return ()

readName :: Name
readName = "read"

readFn :: Operand
readFn = functionRef readName [i32,ptr i8,i32] i32

readDecl :: ModuleBuilder Operand
readDecl = extern readName [i32,ptr i8,i32] i32

writeName :: Name
writeName = "write"

writeFn :: Operand
writeFn = functionRef writeName [i32,ptr i8,i32] i32

writeDecl :: ModuleBuilder Operand
writeDecl = extern writeName [i32,ptr i8,i32] i32

ioLibReadbyteName :: Name
ioLibReadbyteName = "IO.READBYTE"

ioLibReadbyteDecl :: ModuleBuilder Operand
ioLibReadbyteDecl = extern ioLibReadbyteName [pFrameType] void

ioLibReadbyteImpl :: ModuleBuilder Operand
ioLibReadbyteImpl = do
    function ioLibReadbyteName [(pFrameType,NoParameterName)] void $ \ [frame] -> mdo
        buffer <- alloca i8 Nothing 0
        count <- call readFn [intConst 32 0,buffer,intConst 32 1]
        -- ...
        retVoid

ioLibWritebyteName :: Name
ioLibWritebyteName = "IO.WRITEBYTE"

ioLibWritebyteDecl :: ModuleBuilder Operand
ioLibWritebyteDecl = extern ioLibWritebyteName [pFrameType] void

ioLibWritebyteImpl :: ModuleBuilder Operand
ioLibWritebyteImpl = do
    function "IO.WRITEBYTE" [(pFrameType,NoParameterName)] void $ \ [frame] -> mdo
        -- ...
        retVoid
