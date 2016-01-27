module LLVMRuntime
    (LLVMRuntimeType(..),LLVMRuntimeFunc(..))
where

import Control.Monad(foldM,when)

import LLVMGen
    (LLVMGen,Label,Temp,
     newTemp,newLabel,forwardRef,forwardRefTemp,forwardRefLabel,
     writeNewTemp,writeNewLabel,writeCode,
     writeRefCountType,writeOffsetType,writeRTTOffsetType,
     writeTemp,writeLabel,writeLabelRef,writeName,writeBranch)
import Runtime
    (RuntimeType(..),RuntimeFunc(..),Compile,compileError,
     AstType(..),AstFuncSig(..),
     astTypeName,astTypeSize,astTypeImportSize,
     astTypeErrorName,astTypeSourcePos,astTypeIsImport)

data LLVMRuntimeType fwd =
    LLVMRuntimeType [LLVMGen fwd ()] -- declares
                    (LLVMGen fwd Temp) -- new value
                    (Temp -> LLVMGen fwd ()) -- add ref
                    (Temp -> LLVMGen fwd ()) -- remove ref
data LLVMRuntimeFunc fwd =
    LLVMRuntimeFunc [LLVMGen fwd ()] -- declares
                    (LLVMGen fwd ()) -- definition

instance RuntimeType (LLVMRuntimeType fwd) where
    annotateType astType
      | astTypeName astType == "stack" && astTypeIsImport astType =
            return (LLVMRuntimeType stackDecls stackNew stackAddRef stackUnref)
    annotateType astType
      | astTypeName astType == "test" && astTypeIsImport astType =
            return
                (LLVMRuntimeType
                    []
                    (writeNewTemp "select i1 1,i8* null,i8* null;test new\n")
                    (const (writeCode ";test addref\n"))
                    (const (writeCode ";test unref\n")))
    annotateType astType =
        compileError (astTypeSourcePos astType)
                     ("Unknown import " ++ astTypeErrorName astType)

instance RuntimeFunc (LLVMRuntimeFunc fwd) where
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
        return (LLVMRuntimeFunc
                    [writeCode "declare void @llvm.memcpy.p0i8.p0i8.i32(i8*,i8*,i32,i32,i1)"]
                    (pushStack stack bit))
    annotateFunc (AstFuncSig pos name@"popStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack
                                     && astTypeImportSize stack == 1
                                     && astTypeImportSize bit == 0 =
        return (LLVMRuntimeFunc [] (popStack stack bit))
    annotateFunc (AstFuncSig pos name@"isEmptyStack" [(_,stack)] (Just bit))
      | astTypeName stack == "stack" && astTypeIsImport stack
                                     && astTypeImportSize stack == 1
                                     && astTypeImportSize bit == 0 =
        return (LLVMRuntimeFunc [] (isEmptyStack stack bit))
    annotateFunc (AstFuncSig pos name _ _) =
        compileError pos ("Unknown import func '" ++ name ++ "'")

writeNewBitPtr :: Either Temp String -> Temp -> LLVMGen fwd Temp
writeNewBitPtr value index = do
    bitPtr <- writeNewTemp "getelementptr {"
    writeRefCountType ",[0 x i1]},{"
    writeRefCountType ",[0 x i1]}* "
    either (flip writeTemp "") writeCode value
    writeCode ",i32 0,i32 1,"
    writeOffsetType " "
    writeTemp index ""
    return bitPtr

getByte :: AstType -> LLVMGen fwd ()
getByte astType = do
    writeCode "define void @"
    writeName "getByte"
    writeCode "({"
    writeRefCountType ",[0 x i1]}* %value,"
    writeOffsetType " %offset"
    when (astTypeImportSize astType > 0) (do
        writeCode ",i8** %import,"
        writeRTTOffsetType " %importoffset")
    writeCode ") {"
    writeNewLabel
    buffer <- writeNewTemp "alloca i8,i32 1"
    count <- writeNewTemp "call i32 @read(i32 0,i8* "
    writeTemp buffer ",i32 1)"
    cmp <- writeNewTemp "icmp ne i32 1,"
    writeTemp count ""
    when (astTypeSize astType > 8) (do
        index <- writeNewTemp "add "
        writeOffsetType " %offset,8"
        eofBitPtr <- writeNewBitPtr (Right "%value") index
        writeCode " store i1 "
        writeTemp cmp " ,i1* "
        writeTemp eofBitPtr "")
    (eofLabelRef,okLabelRef) <- writeBranch cmp
    eofLabel <- writeNewLabel
    eofLabelRef eofLabel
    writeCode " ret void"
    okLabel <- writeNewLabel
    okLabelRef okLabel
    byte <- writeNewTemp "load i8,i8* "
    writeTemp buffer ""
    mapM_ (\ i -> do
        index <- writeNewTemp "add "
        writeOffsetType (" %offset," ++ show i)
        bitPtr <- writeNewBitPtr (Right "%value") index
        shifted <- writeNewTemp "lshr i8 "
        writeTemp byte ("," ++ show i)
        bit <- writeNewTemp "trunc i8 "
        writeTemp shifted " to i1"
        writeCode " store i1 "
        writeTemp bit ",i1* "
        writeTemp bitPtr "")
        [0..min 7 (astTypeSize astType - 1)]
    writeCode " ret void }"

putByte :: AstType -> LLVMGen fwd ()
putByte astType = do
    writeCode "define void @"
    writeName "putByte"
    writeCode "({"
    writeRefCountType ",[0 x i1]}* %value,"
    writeOffsetType " %offset"
    when (astTypeImportSize astType > 0) (do
        writeCode ",i8** %import,"
        writeRTTOffsetType " %importoffset")
    writeCode ") {"
    writeNewLabel
    buffer <- writeNewTemp "alloca i8,i32 1"
    accumulator <- writeNewTemp "select i1 1,i8 0,i8 0"
    byte <- foldM (\ oldAccumulator i -> do
        index <- writeNewTemp "add "
        writeOffsetType (" %offset," ++ show i)
        bitPtr <- writeNewBitPtr (Right "%value") index
        bit <- writeNewTemp "load i1,i1* "
        writeTemp bitPtr ""
        extended <- writeNewTemp "zext i1 "
        writeTemp bit " to i8"
        shifted <- writeNewTemp "shl i8 "
        writeTemp extended ("," ++ show i)
        newAccumulator <- writeNewTemp "or i8 "
        writeTemp oldAccumulator ","
        writeTemp shifted ""
        return newAccumulator)
        accumulator [0..min 7 (astTypeSize astType - 1)]
    writeCode " store i8 "
    writeTemp byte ",i8* "
    writeTemp buffer ""
    writeNewTemp " call i32 @write(i32 1,i8* "
    writeTemp buffer ",i32 1)"
    writeCode " ret void }"

stackDecls :: [LLVMGen fwd ()]
stackDecls = [
    writeCode "declare i8* @malloc(i32)",
    writeCode "declare void @free(i8*)",
    writeCode "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)",
    unrefStack
    ]

writeStackType :: String -> LLVMGen fwd ()
writeStackType code =
    -- ref count, size, capacity, bits
    writeCode ("{i32,i32,i32,i8*}" ++ code)

stackNew :: LLVMGen fwd Temp
stackNew = do
    offsetPtr <- writeNewTemp "getelementptr "
    writeStackType ","
    writeStackType "* null,i32 1"
    allocSize <- writeNewTemp "ptrtoint "
    writeStackType "* "
    writeTemp offsetPtr " to i32"
    stack <- writeNewTemp "call i8* @malloc(i32 "
    writeTemp allocSize ")"
    writeCode " call @llvm.memset.p0i8.i32(i8* "
    writeTemp stack ",i8 0,i32 0,i32 0,i1 0)"
    return stack

stackAddRef :: Temp -> LLVMGen fwd ()
stackAddRef rawPtr = do
    stack <- writeNewTemp "bitcast i8* "
    writeTemp rawPtr " to "
    writeStackType "*"
    refCountPtr <- writeNewTemp "getelementptr "
    writeStackType ","
    writeStackType "*,i32 0,i32 0"
    refCount <- writeNewTemp "load i32,i32* "
    writeTemp refCountPtr ""
    newRefCount <- writeNewTemp "add i32 1,"
    writeTemp refCount ""
    writeCode " store i32 "
    writeTemp newRefCount ",i32* "
    writeTemp refCountPtr ""

stackUnref :: Temp -> LLVMGen fwd ()
stackUnref rawPtr = do
    writeCode " call void @unrefStack(i8* "
    writeTemp rawPtr ")"

unrefStack :: LLVMGen fwd ()
unrefStack = do
    writeCode "define void @unrefStack(i8* %s) {"
    writeNewLabel
    stack <- writeNewTemp "bitcast i8* %s to "
    writeStackType "*"
    refCountPtr <- writeNewTemp "getelementptr "
    writeStackType ","
    writeStackType "*,i32 0,i32 0"
    refCount <- writeNewTemp "load i32,i32* "
    writeTemp refCountPtr ""
    newRefCount <- writeNewTemp "sub i32 "
    writeTemp refCount ",1"
    writeCode " store i32 "
    writeTemp newRefCount ",i32* "
    writeTemp refCountPtr ""
    cmp <- writeNewTemp "icmp ugt i32 "
    writeTemp newRefCount ",0"
    (aliveLabelRef,deadLabelRef) <- writeBranch cmp
    aliveLabel <- writeNewLabel
    aliveLabelRef aliveLabel
    writeCode " ret void"
    deadLabel <- writeNewLabel
    deadLabelRef deadLabel
    writeCode " call void @free(i8* %s)"
    writeCode " ret void }"

writeValueType :: String -> LLVMGen fwd ()
writeValueType code = do
    writeCode "{"
    writeRefCountType (",[0 x i1]}" ++ code)

pushStack :: AstType -> AstType -> LLVMGen fwd ()
pushStack stackType bitType = do
    writeCode "define void @"
    writeName "pushStack"
    writeCode "("
    writeValueType " %stackvalue,"
    writeOffsetType " %stackoffset,i8** %stackimp,"
    writeRTTOffsetType " %stackimpoffset,"
    writeValueType " %bitvalue,"
    writeOffsetType " %bitoffset) {"
    writeNewLabel
    when (astTypeSize bitType > 0) writeFuncBody
    writeCode " ret void }"
  where
    writeFuncBody = do
        bitPtr <- writeNewTemp "getelementptr "
        writeValueType ","
        writeValueType "* %bitvalue,i32 0,i32 1,"
        writeOffsetType " 0"
        bit <- writeNewTemp "load i1,i1* "
        writeTemp bitPtr ""
        rawPtrPtr <- writeNewTemp "getelementptr i8*,i8** %stackimp,"
        writeRTTOffsetType " %stackimpoffset"
        rawPtr <- writeNewTemp "load i8*,i8** "
        writeTemp rawPtrPtr ""
        stack <- writeNewTemp "bitcast i8* "
        writeTemp rawPtr " to "
        writeStackType "*"
        sizePtr <- writeNewTemp "getelementptr "
        writeStackType ","
        writeStackType "* "
        writeTemp stack ",i32 0,i32 1"
        oldSize <- writeNewTemp "load i32,i32* "
        writeTemp sizePtr ""
        newSize <- writeNewTemp "add i32 1,"
        writeTemp oldSize ""
        writeCode " store i32 "
        writeTemp newSize ",i32* "
        writeTemp sizePtr ""
        capacityPtr <- writeNewTemp "getelementptr "
        writeStackType ","
        writeStackType "* "
        writeTemp stack ",i32 0,i32 2"
        capacity <- writeNewTemp "load i32,i32* "
        writeTemp capacityPtr ""
        cmp <- writeNewTemp "icmp ugt i32 "
        writeTemp newSize ","
        writeTemp capacity ""
        (reallocLabelRef,storeLabelRef) <- writeBranch cmp

        reallocLabel <- writeNewLabel
        reallocLabelRef reallocLabel
        oldByteCapacity <- writeNewTemp "udiv i32 "
        writeTemp capacity ",8"
        newByteCapacity <- writeNewTemp "add i32 "
        writeTemp oldByteCapacity ",128"
        newCapacity <- writeNewTemp "shl i32 "
        writeTemp newByteCapacity ",3"
        allocationPtr <- writeNewTemp "getelementptr "
        writeStackType ","
        writeStackType "* "
        writeTemp stack ",i32 0,i32 3"
        oldAllocation <- writeNewTemp "load i8*,i8** "
        writeTemp allocationPtr ""
        newAllocation <- writeNewTemp "call i8* @malloc(i32 "
        writeTemp newByteCapacity ")"
        writeCode " store i8* "
        writeTemp newAllocation ",i8** "
        writeTemp allocationPtr ""
        cmp <- writeNewTemp "icmp ne i32 "
        writeTemp oldByteCapacity ",0"
        (copyFreeLabelRef,storeLabelRef2) <- writeBranch cmp

        copyFreeLabel <- writeNewLabel
        copyFreeLabelRef copyFreeLabel
        writeCode " call void @llvm.memcpy.p0i8.p0i8.i32(i8* "
        writeTemp newAllocation ",i8* "
        writeTemp oldAllocation ",i32 "
        writeTemp oldByteCapacity ",i32 0,i1 0)"
        writeCode " call void @free(i8* "
        writeTemp oldAllocation ")"
        writeCode " br label "
        storeLabelRef3 <- forwardRefLabel writeLabelRef

        storeLabel <- writeNewLabel
        storeLabelRef storeLabel
        storeLabelRef2 storeLabel
        storeLabelRef3 storeLabel
        byteIndex <- writeNewTemp "udiv i32 "
        writeTemp oldSize ",8"
        bitIndex32 <- writeNewTemp "urem i32 "
        writeTemp oldSize ",8"
        bitIndex <- writeNewTemp "trunc i32 "
        writeTemp bitIndex32 " to i8"
        extBit <- writeNewTemp "zext i1 "
        writeTemp bit " to i8"
        shiftedBit <- writeNewTemp "shl i8 "
        writeTemp extBit ","
        writeTemp bitIndex ""
        bitMask <- writeNewTemp "xor i8 "
        writeTemp shiftedBit ",255"
        stackArrayPtr <- writeNewTemp "getelementptr "
        writeStackType ","
        writeStackType "* "
        writeTemp stack ",i32 0,i32 3"
        stackArray <- writeNewTemp "load i8*,i8** "
        writeTemp stackArrayPtr ""
        stackBytePtr <- writeNewTemp "getelementptr i8,i8* "
        writeTemp stackArray ",i32 "
        writeTemp byteIndex ""
        oldStackByte <- writeNewTemp "load i8,i8* "
        writeTemp stackBytePtr ""
        maskedStackByte <- writeNewTemp "and i8 "
        writeTemp bitMask ","
        writeTemp oldStackByte ""
        newStackByte <- writeNewTemp "or i8 "
        writeTemp maskedStackByte ","
        writeTemp shiftedBit ""
        writeCode " store i8 "
        writeTemp newStackByte ",i8* "
        writeTemp stackBytePtr ""

popStack :: AstType -> AstType -> LLVMGen fwd ()
popStack stackType bitType = do
    writeCode "define {"
    writeValueType "*,"
    writeOffsetType ",i8**,"
    writeRTTOffsetType "} @"
    writeName "pushStack"
    writeCode "("
    writeValueType "* %stackvalue,"
    writeOffsetType " %stackoffset,i8** %stackimp,"
    writeRTTOffsetType " %stackimpoffset,{i8*,i8**} %retval) {"
    writeNewLabel
    retValRawPtr <- writeNewTemp "extractvalue {i8*,i8**} %retval,0"
    retValValue <- writeNewTemp "bitcast i8* "
    writeTemp retValRawPtr " to "
    writeValueType "*"
    rawPtrPtr <- writeNewTemp "getelementptr i8*,i8** %stackimp,"
    writeRTTOffsetType " %stackimpoffset"
    rawPtr <- writeNewTemp "load i8*,i8** "
    writeTemp rawPtrPtr ""
    stack <- writeNewTemp "bitcast i8* "
    writeTemp rawPtr " to "
    writeStackType "*"
    sizePtr <- writeNewTemp "getelementptr "
    writeStackType ","
    writeStackType "* "
    writeTemp stack ",i32 0,i32 1"
    oldSize <- writeNewTemp "load i32,i32* "
    writeTemp sizePtr ""
    cmp <- writeNewTemp "icmp ugt i32 "
    writeTemp oldSize ",0"
    (popLabelRef,retLabelRef) <- writeBranch cmp
    popLabel <- writeNewLabel
    popLabelRef popLabel
    newSize <- writeNewTemp "sub i32 "
    writeTemp oldSize ",1"
    writeCode " store i32 "
    writeTemp newSize ",i32* "
    writeTemp sizePtr ""
    when (astTypeSize bitType > 0) (do
        byteIndex <- writeNewTemp "udiv i32 "
        writeTemp newSize ",8"
        bitIndex <- writeNewTemp "urem i32 "
        writeTemp newSize ",8"
        stackArrayPtr <- writeNewTemp "getelementptr "
        writeStackType ","
        writeStackType "* "
        writeTemp stack ",i32 0,i32 3"
        stackArray <- writeNewTemp "load i8*,i8** "
        writeTemp stackArrayPtr ""
        stackBytePtr <- writeNewTemp "getelementptr i8,i8* "
        writeTemp stackArray ",i32 "
        writeTemp byteIndex ""
        stackByte <- writeNewTemp "load i8,i8* "
        writeTemp stackBytePtr ""
        stackByte32 <- writeNewTemp "zext i8 "
        writeTemp stackByte " to i32"
        shiftedByte <- writeNewTemp "lshr i32 "
        writeTemp stackByte32 ","
        writeTemp bitIndex ""
        bit <- writeNewTemp "trunc i32 "
        writeTemp shiftedByte " to i1"
        bitPtr <- writeNewTemp "getelementptr "
        writeValueType ","
        writeValueType "* "
        writeTemp retValValue ",i32 0,i32 1,"
        writeOffsetType " 0"
        writeCode " store i8 "
        writeTemp bit ",i8* "
        writeTemp bitPtr "")
    writeCode " br label "
    retLabelRef2 <- forwardRefLabel writeLabelRef
    retLabel <- writeNewLabel
    retLabelRef retLabel
    retLabelRef2 retLabel
    retVal1 <- writeNewTemp "insertvalue {"
    writeValueType "*,"
    writeOffsetType "} undef,"
    writeValueType "* "
    writeTemp retValValue ",0"
    retVal <- writeNewTemp "insertvalue {"
    writeValueType "*,"
    writeOffsetType "} "
    writeTemp retVal1 ","
    writeOffsetType " 0,1"
    writeCode " ret {"
    writeValueType "*,"
    writeOffsetType "} "
    writeTemp retVal " }"

isEmptyStack :: AstType -> AstType -> LLVMGen fwd ()
isEmptyStack stackType bitType = do
    writeCode "define {"
    writeValueType "*,"
    writeOffsetType ",i8**,"
    writeRTTOffsetType "} @"
    writeName "isEmptyStack"
    writeCode "("
    writeValueType "* %stackvalue,"
    writeOffsetType " %stackoffset,i8** %stackimp,"
    writeRTTOffsetType " %stackimpoffset,{i8*,i8**} %retval) {"
    writeNewLabel
    retValRawPtr <- writeNewTemp "extractvalue {i8*,i8**} %retval,0"
    retValValue <- writeNewTemp "bitcast i8* "
    writeTemp retValRawPtr " to "
    writeValueType "*"
    rawPtrPtr <- writeNewTemp "getelementptr i8*,i8** %stackimp,"
    writeRTTOffsetType " %stackimpoffset"
    rawPtr <- writeNewTemp "load i8*,i8** "
    writeTemp rawPtrPtr ""
    stack <- writeNewTemp "bitcast i8* "
    writeTemp rawPtr " to "
    writeStackType "*"
    sizePtr <- writeNewTemp "getelementptr "
    writeStackType ","
    writeStackType "* "
    writeTemp stack ",i32 0,i32 1"
    size <- writeNewTemp "load i32,i32* "
    writeTemp sizePtr ""
    bit <- writeNewTemp "icmp eq i32 "
    writeTemp size ",0"
    when (astTypeSize bitType > 0) (do
        bitPtr <- writeNewTemp "getelementptr "
        writeValueType ","
        writeValueType "* "
        writeTemp retValValue ",i32 0,i32 1,"
        writeOffsetType " 0"
        writeCode " store i8 "
        writeTemp bit ",i8* "
        writeTemp bitPtr "")
    retVal1 <- writeNewTemp "insertvalue {"
    writeValueType "*,"
    writeOffsetType "} undef,"
    writeValueType "* "
    writeTemp retValValue ",0"
    retVal <- writeNewTemp "insertvalue {"
    writeValueType "*,"
    writeOffsetType "} "
    writeTemp retVal1 ","
    writeOffsetType " 0,1"
    writeCode " ret {"
    writeValueType "*,"
    writeOffsetType "} "
    writeTemp retVal " }"
