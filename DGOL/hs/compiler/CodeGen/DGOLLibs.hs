{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.DGOLLibs(dgolLibDefs)
where

import Prelude hiding(and,or)

import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand)
import LLVM.AST.Type(Type,void,i8,i32,ptr)
import LLVM.IRBuilder.Instruction(alloca,and,bitcast,br,condBr,gep,icmp,load,or,phi,ret,retVoid,store)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),extern,function)
import LLVM.IRBuilder.Monad(IRBuilderT,block)

import CodeGen.Runtime(newNode,hasEdge,removeEdge,addEdge)
import CodeGen.Types(pFrameType,pNodeType,ppNodeType,pppNodeType)
import CodeGen.Util(
    intConst,nullConst,eq,uge,
    functionRef,
    call)

dgolLibDefs :: String -> Maybe (ModuleBuilder ())
dgolLibDefs name
  | name == "IO" = Just ioLibDefs
  | otherwise = Nothing

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

ioLibReadbyteImpl :: ModuleBuilder Operand
ioLibReadbyteImpl = function "IO.READBYTE" [(pFrameType,NoParameterName)] void $ \ [frame] -> mdo
    argCountPtr <- gep frame [intConst 32 0,intConst 32 5]
    argCount_workaround <- load argCountPtr 0
    argCount <- bitcast argCount_workaround i32
    argArrayPtr <- gep frame [intConst 32 0,intConst 32 6]
    argArray_workaround <- load argArrayPtr 0
    argArray <- bitcast argArray_workaround pppNodeType
    buffer <- alloca i8 Nothing 0
    count <- call readFn [intConst 32 0,buffer,intConst 32 1]
    eof <- icmp eq count (intConst 32 0)
    condBr eof isEof notEof

    isEof <- block
    argsAddEdge frame argCount argArray 0 1 done
        
    notEof <- block
    argsRemoveEdge argCount argArray 0 1 doBit1

    doBit1 <- block
    byte <- load buffer 0
    bit1 <- and byte (intConst 8 1)
    bit1Check <- icmp eq bit1 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit1Check 2 doBit2

    doBit2 <- block
    bit2 <- and byte (intConst 8 2)
    bit2Check <- icmp eq bit2 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit2Check 3 doBit3

    doBit3 <- block
    bit3 <- and byte (intConst 8 4)
    bit3Check <- icmp eq bit3 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit3Check 4 doBit4

    doBit4 <- block
    bit4 <- and byte (intConst 8 8)
    bit4Check <- icmp eq bit4 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit4Check 5 doBit5

    doBit5 <- block
    bit5 <- and byte (intConst 8 16)
    bit5Check <- icmp eq bit5 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit5Check 6 doBit6

    doBit6 <- block
    bit6 <- and byte (intConst 8 32)
    bit6Check <- icmp eq bit6 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit6Check 7 doBit7

    doBit7 <- block
    bit7 <- and byte (intConst 8 64)
    bit7Check <- icmp eq bit7 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit7Check 8 doBit8

    doBit8 <- block
    bit8 <- and byte (intConst 8 128)
    bit8Check <- icmp eq bit8 (intConst 8 0)
    setReadbyteResult frame argCount argArray bit8Check 9 done

    done <- block
    retVoid

ioLibWritebyteImpl :: ModuleBuilder Operand
ioLibWritebyteImpl = function "IO.WRITEBYTE" [(pFrameType,NoParameterName)] void $ \ [frame] -> mdo
    argCountPtr <- gep frame [intConst 32 0,intConst 32 5]
    argCount <- load argCountPtr 0
    argArrayPtr <- gep frame [intConst 32 0,intConst 32 6]
    argArray <- load argArrayPtr 0
    argsHaveEdge argCount argArray 0 1 bit1_1 bit1_0

    bit1_0 <- block
    br bit2

    bit1_1 <- block
    br bit2

    bit2 <- block
    byte1 <- phi [(intConst 8 0,bit1_0),(intConst 8 1,bit1_1)]
    argsHaveEdge argCount argArray 0 2 bit2_1 bit2_0

    bit2_0 <- block
    br bit3

    bit2_1 <- block
    byte2_1 <- or byte1 (intConst 8 2)
    br bit3

    bit3 <- block
    byte2 <- phi [(byte1,bit2_0),(byte2_1,bit2_1)]
    argsHaveEdge argCount argArray 0 3 bit3_1 bit3_0

    bit3_0 <- block
    br bit4

    bit3_1 <- block
    byte3_1 <- or byte2 (intConst 8 4)
    br bit4

    bit4 <- block
    byte3 <- phi [(byte2,bit3_0),(byte3_1,bit3_1)]
    argsHaveEdge argCount argArray 0 4 bit4_1 bit4_0

    bit4_0 <- block
    br bit5

    bit4_1 <- block
    byte4_1 <- or byte3 (intConst 8 8)
    br bit5

    bit5 <- block
    byte4 <- phi [(byte3,bit4_0),(byte4_1,bit4_1)]
    argsHaveEdge argCount argArray 0 5 bit5_1 bit5_0

    bit5_0 <- block
    br bit6

    bit5_1 <- block
    byte5_1 <- or byte4 (intConst 8 16)
    br bit6

    bit6 <- block
    byte5 <- phi [(byte4,bit5_0),(byte5_1,bit5_1)]
    argsHaveEdge argCount argArray 0 6 bit6_1 bit6_0

    bit6_0 <- block
    br bit7

    bit6_1 <- block
    byte6_1 <- or byte5 (intConst 8 32)
    br bit7

    bit7 <- block
    byte6 <- phi [(byte5,bit6_0),(byte6_1,bit6_1)]
    argsHaveEdge argCount argArray 0 7 bit7_1 bit7_0

    bit7_0 <- block
    br bit8

    bit7_1 <- block
    byte7_1 <- or byte6 (intConst 8 64)
    br bit8

    bit8 <- block
    byte7 <- phi [(byte6,bit7_0),(byte7_1,bit7_1)]
    argsHaveEdge argCount argArray 0 8 bit8_1 bit8_0

    bit8_0 <- block
    br writebyte

    bit8_1 <- block
    byte8_1 <- or byte7 (intConst 8 128)
    br writebyte

    writebyte <- block
    byte <- phi [(byte7,bit8_0),(byte8_1,bit8_1)]
    buffer <- alloca i8 Nothing 0
    store buffer 0 byte
    call writeFn [intConst 32 1,buffer,intConst 32 1]
    retVoid

argsHaveEdge :: Operand -> Operand -> Integer -> Integer -> Name -> Name -> IRBuilderT ModuleBuilder ()
argsHaveEdge argCount argArray argIndex0 argIndex1 yesLabel noLabel = mdo
    argIndex0Check <- icmp uge (intConst 32 argIndex0) argCount
    condBr argIndex0Check noLabel checkArgIndex1

    checkArgIndex1 <- block
    argIndex1Check <- icmp uge (intConst 32 argIndex1) argCount
    condBr argIndex1Check noLabel checkArg0

    checkArg0 <- block
    arg0Ptr <- gep argArray [intConst 32 argIndex0]
    arg0 <- load arg0Ptr 0
    arg0Check <- icmp eq arg0 (nullConst ppNodeType)
    condBr arg0Check noLabel checkArg1

    checkArg1 <- block
    arg1Ptr <- gep argArray [intConst 32 argIndex1]
    arg1 <- load arg1Ptr 0
    arg1Check <- icmp eq arg1 (nullConst ppNodeType)
    condBr arg1Check noLabel checkNode0

    checkNode0 <- block
    node0Ptr <- gep arg0 [intConst 32 0]
    node0 <- load node0Ptr 0
    node0Check <- icmp eq node0 (nullConst pNodeType)
    condBr node0Check noLabel checkNode1

    checkNode1 <- block
    node1Ptr <- gep arg1 [intConst 32 0]
    node1 <- load node1Ptr 0
    node1Check <- icmp eq node1 (nullConst pNodeType)
    condBr node1Check noLabel checkEdge

    checkEdge <- block
    edgeCheck <- call hasEdge [node0,node1]
    condBr edgeCheck yesLabel noLabel

argsRemoveEdge :: Operand -> Operand -> Integer -> Integer -> Name -> IRBuilderT ModuleBuilder ()
argsRemoveEdge argCount argArray argIndex0 argIndex1 label = mdo
    argIndex0Check <- icmp uge (intConst 32 argIndex0) argCount
    condBr argIndex0Check label checkArgIndex1

    checkArgIndex1 <- block
    argIndex1Check <- icmp uge (intConst 32 argIndex1) argCount
    condBr argIndex1Check label checkArg0

    checkArg0 <- block
    arg0Ptr <- gep argArray [intConst 32 argIndex0]
    arg0 <- load arg0Ptr 0
    arg0Check <- icmp eq arg0 (nullConst ppNodeType)
    condBr arg0Check label checkArg1

    checkArg1 <- block
    arg1Ptr <- gep argArray [intConst 32 argIndex1]
    arg1 <- load arg1Ptr 0
    arg1Check <- icmp eq arg1 (nullConst ppNodeType)
    condBr arg1Check label checkNode0

    checkNode0 <- block
    node0Ptr <- gep arg0 [intConst 32 0]
    node0 <- load node0Ptr 0
    node0Check <- icmp eq node0 (nullConst pNodeType)
    condBr node0Check label checkNode1

    checkNode1 <- block
    node1Ptr <- gep arg1 [intConst 32 0]
    node1 <- load node1Ptr 0
    node1Check <- icmp eq node1 (nullConst pNodeType)
    condBr node1Check label doRemove

    doRemove <- block
    call removeEdge [node0,node1]
    br label

argsAddEdge :: Operand -> Operand -> Operand -> Integer -> Integer -> Name -> IRBuilderT ModuleBuilder ()
argsAddEdge frame argCount argArray argIndex0 argIndex1 label = mdo
    argIndex0Check <- icmp uge (intConst 32 argIndex0) argCount
    condBr argIndex0Check label checkArgIndex1

    checkArgIndex1 <- block
    argIndex1Check <- icmp uge (intConst 32 argIndex1) argCount
    condBr argIndex1Check label checkArg0

    checkArg0 <- block
    arg0Ptr <- gep argArray [intConst 32 argIndex0]
    arg0 <- load arg0Ptr 0
    arg0Check <- icmp eq arg0 (nullConst ppNodeType)
    condBr arg0Check label checkArg1

    checkArg1 <- block
    arg1Ptr <- gep argArray [intConst 32 argIndex1]
    arg1 <- load arg1Ptr 0
    arg1Check <- icmp eq arg1 (nullConst ppNodeType)
    condBr arg1Check label checkNode0

    checkNode0 <- block
    node0Ptr <- gep arg0 [intConst 32 0]
    node0Maybe <- load node0Ptr 0
    node0Check <- icmp eq node0Maybe (nullConst pNodeType)
    condBr node0Check makeNode0 checkNode1

    makeNode0 <- block
    node0New <- call newNode [frame]
    store node0Ptr 0 node0
    br checkNode1

    checkNode1 <- block
    node0 <- phi [(node0Maybe,checkNode0),(node0New,makeNode0)]
    node1Ptr <- gep arg1 [intConst 32 0]
    node1Maybe <- load node1Ptr 0
    node1Check <- icmp eq node1 (nullConst pNodeType)
    condBr node1Check makeNode1 doAdd

    makeNode1 <- block
    node1New <- call newNode [frame]
    store node1Ptr 0 node1
    br doAdd

    doAdd <- block
    node1 <- phi [(node1Maybe,checkNode1),(node1New,makeNode1)]
    call addEdge [node0,node1]
    br label

setReadbyteResult :: Operand -> Operand -> Operand -> Operand -> Integer -> Name -> IRBuilderT ModuleBuilder ()
setReadbyteResult frame argCount argArray result index label = mdo
    condBr result doRemove doAdd

    doRemove <- block
    argsRemoveEdge argCount argArray 0 index label

    doAdd <- block
    argsAddEdge frame argCount argArray 0 index label
