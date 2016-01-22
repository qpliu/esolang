module LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..))
where

import Control.Monad(foldM,when)

import LLVMGen
    (CodeGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,
     writeNewTemp,writeNewLabel,writeCode,writeRefCountType,writeOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,writeBranch)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),Compile,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSize,astTypeImportSize,
     astTypeErrorName,astTypeSourcePos,astTypeIsImport)

data LLVMRuntimeType =
    LLVMRuntimeType [CodeGen ()] -- declares
                    (CodeGen Temp) -- new value
                    (Temp -> CodeGen ()) -- add ref
                    (Temp -> CodeGen ()) -- remove ref
data LLVMRuntimeFunc =
    LLVMRuntimeFunc [CodeGen ()] -- declares
                    (CodeGen ()) -- definition

instance RuntimeType LLVMRuntimeType where
    annotateType astType
      | astTypeName astType == "stack" && astTypeIsImport astType =
            compileError (astTypeSourcePos astType)
                         ("Unknown import " ++ astTypeErrorName astType)
    annotateType astType =
        compileError (astTypeSourcePos astType)
                     ("Unknown import " ++ astTypeErrorName astType)

instance RuntimeFunc LLVMRuntimeFunc where
    annotateFunc (AstFuncSig pos name@"getByte" [(_,astType)] Nothing)
      | astTypeImportSize astType == 0 =
        return (LLVMRuntimeFunc [writeCode "declare i32 @read(i32,i8*,i32)"]
                                (getByte astType))
    annotateFunc (AstFuncSig pos name@"putByte" [(_,astType)] Nothing)
      | astTypeImportSize astType == 0 =
        return (LLVMRuntimeFunc [writeCode "declare i32 @write(i32,i8*,i32)"]
                                (putByte astType))
    annotateFunc (AstFuncSig pos name@"pushStack" [(_,stack),(_,bit)] Nothing)
      | astTypeName stack == "stack" && astTypeIsImport stack
                                     && astTypeImportSize stack == 1
                                     && astTypeImportSize bit == 0 =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"popStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack
                                     && astTypeImportSize stack == 1
                                     && astTypeImportSize bit == 0 =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name@"isEmptyStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack
                                     && astTypeImportSize stack == 1
                                     && astTypeImportSize bit == 0 =
        compileError pos ("Unknown import func '" ++ name ++ "'")
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")

writeNewBitPtr :: Either Temp String -> Temp -> CodeGen Temp
writeNewBitPtr value index = do
    bitPtr <- writeNewTemp
    writeCode "getelementptr {"
    writeRefCountType
    writeCode ",[0 x i1]},{"
    writeRefCountType
    writeCode ",[0 x i1]}* "
    either writeTemp writeCode value
    writeCode ",i32 0,i32 1,"
    writeOffsetType
    writeCode " "
    writeTemp index
    return bitPtr

getByte :: AstType -> CodeGen ()
getByte astType = do
    writeCode "define void @"
    writeName "getByte"
    writeCode "({"
    writeRefCountType
    writeCode ",[0 x i1]}* %value,"
    writeOffsetType
    writeCode " %offset"
    when (astTypeImportSize astType > 0)
         (writeCode (", [" ++ show (astTypeImportSize astType)
                           ++ " x i8*] %import"))
    writeCode ") {"
    writeNewLabel
    buffer <- writeNewTemp
    writeCode "alloca i8,i32 1"
    count <- writeNewTemp
    writeCode "call i32 @read(i32 0,i8* "
    writeTemp buffer
    writeCode ",i32 1)"
    cmp <- writeNewTemp
    writeCode "icmp ne i32 1,"
    writeTemp count
    when (astTypeSize astType > 8) (do
        index <- writeNewTemp
        writeCode "add "
        writeOffsetType
        writeCode " %offset,8"
        eofBitPtr <- writeNewBitPtr (Right "%value") index
        writeCode " store i1 "
        writeTemp cmp
        writeCode " ,i1* "
        writeTemp eofBitPtr)
    (eofLabelRef,okLabelRef) <- writeBranch cmp
    eofLabel <- writeNewLabel
    eofLabelRef eofLabel
    writeCode " ret void"
    okLabel <- writeNewLabel
    okLabelRef okLabel
    byte <- writeNewTemp
    writeCode "load i8,i8* "
    writeTemp buffer
    mapM_ (\ i -> do
        index <- writeNewTemp
        writeCode "add "
        writeOffsetType
        writeCode (" %offset," ++ show i)
        bitPtr <- writeNewBitPtr (Right "%value") index
        shifted <- writeNewTemp
        writeCode "lshr i8 "
        writeTemp byte
        writeCode ("," ++ show i)
        bit <- writeNewTemp
        writeCode "trunc i8 "
        writeTemp shifted
        writeCode " to i1"
        writeCode " store i1 "
        writeTemp bit
        writeCode ",i1* "
        writeTemp bitPtr)
        [0..min 7 (astTypeSize astType - 1)]
    writeCode " ret void }"

putByte :: AstType -> CodeGen ()
putByte astType = do
    writeCode "define void @"
    writeName "putByte"
    writeCode "({"
    writeRefCountType
    writeCode ",[0 x i1]}* %value,"
    writeOffsetType
    writeCode " %offset"
    when (astTypeImportSize astType > 0)
         (writeCode (", [" ++ show (astTypeImportSize astType)
                           ++ " x i8*] %import"))
    writeCode ") {"
    writeNewLabel
    buffer <- writeNewTemp
    writeCode "alloca i8,i32 1"
    accumulator <- writeNewTemp
    writeCode "select i1 1,i8 0,i8 0"
    byte <- foldM (\ oldAccumulator i -> do
        index <- writeNewTemp
        writeCode "add "
        writeOffsetType
        writeCode (" %offset," ++ show i)
        bitPtr <- writeNewBitPtr (Right "%value") index
        bit <- writeNewTemp
        writeCode "load i1,i1* "
        writeTemp bitPtr
        extended <- writeNewTemp
        writeCode "zext i1 "
        writeTemp bit
        writeCode " to i8"
        shifted <- writeNewTemp
        writeCode "shl i8 "
        writeTemp extended
        writeCode ("," ++ show i)
        newAccumulator <- writeNewTemp
        writeCode "or i8 "
        writeTemp oldAccumulator
        writeCode ","
        writeTemp extended
        return newAccumulator)
        accumulator [0..min 7 (astTypeSize astType - 1)]
    writeCode " store i8 "
    writeTemp byte
    writeCode ",i8* "
    writeTemp buffer
    writeNewTemp
    writeCode " call i32 @write(i32 1,i8* "
    writeTemp buffer
    writeCode ",i32 1)"
    writeCode " ret void }"
