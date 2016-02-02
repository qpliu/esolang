module CodeGen
    (codeGen,genMain)
where

import Control.Monad(foldM,unless,zipWithM_)
import Data.Char(isAlphaNum,isAscii,ord)
import Data.Set(Set)
import qualified Data.Set as Set

import Ast(Identifier(..),Param(..),Def(..),Expr(..),Func)
import GenLLVM
    (GenLLVM,Local,Label,genLLVM,
     writeCode,writeLocal,writeLabelRef,
     writeNewLocal,writeNewLabel,writeNewLabelBack,
     writeForwardRefLabel,writeBranch,writePhi)

codeGen :: [Func] -> String
codeGen funcs =
    (concatMap writeLiteral . Set.toList . foldl collectLiterals Set.empty)
            funcs
        ++ concatMap genLLVM [ -- runtime functions
            writeDecls,
            writeUnrefDefn,
            writeEvalLiteralValueDefn,
            writeFreeEvalParamLiteralValueDefn,
            writeEvalConcatValueDefn,
            writeFreeEvalParamConcatValueDefn,
            writeEvalFileValueDefn,
            writeFreeEvalParamFileValueDefn,
            writeFreeEvalParamFuncValueDefn
            ]
        ++ (concatMap genLLVM . map writeFunc) funcs
        ++ (concatMap genLLVM . map writeEvalFuncValue) funcs

collectLiterals :: Set [Bool] -> Func -> Set [Bool]
collectLiterals literals (_,defs) = foldl collectDefLiterals literals defs
  where
    collectDefLiterals literals (Def _ expr) =
        collectExprLiterals literals expr
    collectExprLiterals literals (ExprLiteral _ bits) =
        Set.insert bits literals
    collectExprLiterals literals (ExprFuncall _ exprs) =
        foldl collectExprLiterals literals exprs
    collectExprLiterals literals (ExprConcat expr1 expr2) =
        collectExprLiterals (collectExprLiterals literals expr1) expr2
    collectLiterals literals _ = literals

writeLiteral :: [Bool] -> String
writeLiteral bits =
    literalName bits ++ " = private constant " ++ literalType bits
                     ++ " [" ++ drop 1 (concatMap writeBit bits) ++ "]"
  where
    writeBit bit = ",i1 " ++ if bit then "1" else "0"

literalName :: [Bool] -> String
literalName bits = "@L" ++ map (\ bit -> if bit then '1' else '0') bits

literalType :: [Bool] -> String
literalType bits = "[" ++ show (length bits) ++ " x i1]"

writeName :: String -> GenLLVM ()
writeName name = writeCode ("@_" ++ quoteName name)

writeEvalFuncName :: String -> GenLLVM ()
writeEvalFuncName name = writeCode ("@evalFunc_" ++ quoteName name)

quoteName :: String -> String
quoteName name = concatMap quote name
  where
    quote c | isAscii c && isAlphaNum c = [c]
            | otherwise = "_" ++ show (ord c) ++ "_"

writeValueType :: String -> GenLLVM ()
writeValueType code =
    -- 0:i32 ref count
    -- 1:i2 status (0:0,1:1,2:nil,3:unevaluated)
    -- 2:i8* next/eval param
    -- 3:{i2,i8*}(i8*)* eval
    -- 4:void(i8*)* free eval param
    writeCode ("{i32,i2,i8*,{i2,i8*}(i8*)*,void(i8*)*}" ++ code)

writeFunc :: Func -> GenLLVM ()
writeFunc (name,Def params _:_) = do
    writeCode "define fastcc "
    writeValueType "* "
    writeName name
    writeCode "("
    zipWithM_ (\ comma i -> do
            writeCode comma
            writeValueType ("* %a" ++ show i))
        ("":repeat ",") [0 .. length params - 1]
    writeCode ") {"
    writeNewLabel
    value <- writeAllocateNewValue 3
    sizePtr <- writeNewLocal "getelementptr "
    writeFuncValueEvalParamType ","
    writeFuncValueEvalParamType
        ("* nil,i32 0,i32 1,i32 " ++ show (length params))
    size <- writeNewLocal "ptrtoint "
    writeValueType "** "
    writeLocal sizePtr " to i32"
    rawPtr <- writeNewLocal "call i8* @malloc(i32 "
    writeLocal size ")"
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeFuncValueEvalParamType "*"
    argCountPtr <- writeNewLocal "getelementptr "
    writeFuncValueEvalParamType ","
    writeFuncValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    writeCode (" store i32 " ++ show (length params) ++ ",i32* ")
    writeLocal argCountPtr ""
    mapM_ (\ i -> do
            argPtr <- writeNewLocal "getelementptr "
            writeFuncValueEvalParamType ","
            writeFuncValueEvalParamType
                ("* %a" ++ show i ++ ",i32 0,i32 1,i32 " ++ show i)
            writeCode " store "
            writeValueType ("* %a" ++ show i ++ ",")
            writeValueType "** "
            writeLocal argPtr "")
        [0 .. length params - 1]

    evalParamPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 2"
    writeCode " store i8* "
    writeLocal rawPtr ",i8** "
    writeLocal evalParamPtr ""

    evalFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 3"
    writeCode " store {i2,i8*}(i8*)* "
    writeEvalFuncName name
    writeCode ",{i2,i8*}(i8*)** "
    writeLocal evalFuncPtr ""

    freeEvalParamFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 4"
    writeCode " store void(i8*)* @freeEvalParamFunc,void(i8*)** "
    writeLocal freeEvalParamFuncPtr ""

    writeCode " ret "
    writeValueType "* "
    writeLocal value ""
    writeCode " }"

writeFuncValueEvalParamType :: String -> GenLLVM ()
writeFuncValueEvalParamType code = do
    -- 0:i32 number of arguments
    -- 1:[0 x {i32,i2,i8*,{i2,i8*}(i8*)*,void(i8*)*}] arguments
    writeCode "{i32,[0 x "
    writeValueType ("*]}" ++ code)

writeFreeEvalParamFuncValueDefn :: GenLLVM ()
writeFreeEvalParamFuncValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamFunc(i8* %evalParam) {"
    entryLabel <- writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeFuncValueEvalParamType "*"
    argCountPtr <- writeNewLocal "getelementptr "
    writeFuncValueEvalParamType ","
    writeFuncValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    argCount <- writeNewLocal "load i32,i32* "
    writeLocal argCountPtr ""

    loopLabel <- writeNewLabel
    (argIndex,argIndexPhiRef) <-
        writePhi (writeCode "i32") (Left "0") entryLabel
    cmp <- writeNewLocal "cmp ult i32 "
    writeLocal argIndex ","
    writeLocal argCount ""
    (continueLoopLabelRef,doneLabelRef) <- writeBranch cmp

    continueLoopLabel <- writeNewLabelBack [continueLoopLabelRef]
    argPtr <- writeNewLocal "getelementptr "
    writeFuncValueEvalParamType ","
    writeFuncValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1,i32 "
    writeLocal argIndex ""
    arg <- writeNewLocal "load "
    writeValueType ","
    writeValueType "* "
    writeLocal argPtr ""
    writeUnref (Left arg)
    newArgIndex <- writeNewLocal "add i32 1,"
    writeLocal argIndex ""
    argIndexPhiRef newArgIndex continueLoopLabel
    writeCode " br label "
    writeLabelRef loopLabel ""

    writeNewLabelBack [doneLabelRef]
    writeCode " call void @free(i8* %evalParam)"
    writeCode " ret void"
    writeCode " }"

writeEvalFuncValue :: Func -> GenLLVM ()
writeEvalFuncValue (name,defs) = do
    writeCode "define private fastcc {i2,i8*} "
    writeEvalFuncName name
    writeCode "(i8* %evalParam) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeFuncValueEvalParamType "*"

    let Def params _:_ = defs
    args <- mapM (\ i -> do
            argPtr <- writeNewLocal "getelementptr "
            writeFuncValueEvalParamType ","
            writeFuncValueEvalParamType "* "
            writeLocal evalParam (",i32 0,i32 1,i32 " ++ show i)
            arg <- writeNewLocal "load "
            writeValueType "*,"
            writeValueType "** "
            writeLocal argPtr ""
            return arg)
        [0 .. length params - 1]
    writeCode " call void @free(i8* %evalParam)"

    writeCode " br label "
    labelRef <- writeForwardRefLabel
    labelRefs <- foldM (\ labelRefs def -> do
            writeNewLabelBack labelRefs
            writeDef def args)
        [labelRef] defs
    unless (null labelRefs) (do
        writeNewLabelBack labelRefs
        writeCode " call void @abort() noreturn")
    writeCode " }"

writeDef :: Def -> [Local] -> GenLLVM [Label -> GenLLVM ()]
writeDef (Def params expr) args = do
    (bindings,_,nextDefLabelRefs) <-
        foldM writeBindParam ([],0,[]) (zip args params)
    mapM_ writeAddRef bindings
    mapM_ (writeUnref . Left) args
    value <- writeExpr bindings expr
    (status,nextValue) <- writeForceEval value
    writeAddRef nextValue
    writeUnref (Left value)
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal nextValue " to i8*"
    retValue1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal status ",0"
    retValue <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retValue1 ",i8* "
    writeLocal rawPtr ",1"
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""
    return nextDefLabelRefs

writeBindParam :: ([Local],Int,[Label -> GenLLVM ()]) -> (Local,Param)
               -> GenLLVM ([Local],Int,[Label -> GenLLVM ()])
writeBindParam (bindings,index,nextDefLabelRefs) (arg,param) = w param
  where
    w (ParamBound _ bits _) = do
        (value,labelRefs) <- foldM writeCheckBit (arg,[]) bits
        return (bindings ++ [value],index+1,labelRefs ++ nextDefLabelRefs)
    w (ParamIgnored _ bits) = do
        (value,labelRefs) <- foldM writeCheckBit (arg,[]) bits
        return (bindings,index+1,labelRefs ++ nextDefLabelRefs)
    w (ParamLiteral _ bits) = do
        (value,labelRefs) <- foldM writeCheckBit (arg,[]) bits
        (valueState,_) <- writeForceEval value
        cmp <- writeNewLocal "icmp ne i2 2,"
        writeLocal valueState ""
        (tryNextDefRef,bindSuccessRef) <- writeBranch cmp
        writeNewLabelBack [bindSuccessRef]
        return (bindings,index+1,tryNextDefRef : labelRefs ++ nextDefLabelRefs)
    writeCheckBit (value,labelRefs) bit = do
        (valueState,nextValue) <- writeForceEval value
        cmp <- writeNewLocal "icmp eq i2 "
        writeLocal valueState (if bit then ",1" else ",0")
        (matchRef,notMatchRef) <- writeBranch cmp
        writeNewLabelBack [matchRef]
        return (nextValue,notMatchRef:labelRefs)

writeExpr :: [Local] -> Expr -> GenLLVM Local
writeExpr bindings expr = w expr
  where
    w (ExprLiteral _ bits) = do
        writeNewConstantLiteralValue bits
    w (ExprBound index) = do
        writeAddRef (bindings !! index)
        return (bindings !! index)
    w (ExprFuncall (Identifier _ name) exprs) = do
        args <- mapM (writeExpr bindings) exprs
        value <- writeNewLocal "call fastcc "
        writeValueType "* "
        writeName name
        writeCode "("
        zipWithM_ (\ comma arg -> do
                writeCode comma
                writeValueType "* "
                writeLocal arg "")
            ("":repeat ",") args
        writeCode ")"
        return value
    w (ExprConcat expr1 expr2) = do
        value1 <- writeExpr bindings expr1
        value2 <- writeExpr bindings expr2
        writeNewConcatValue value1 value2

writeForceEval :: Local -> GenLLVM (Local,Local)
writeForceEval value = do
    evalParamPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 2"
    evalParam <- writeNewLocal "load i8*,i8** "
    evalPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 3"
    eval <- writeNewLocal "load {i2,i8*}(i8*)*,{i2,i8*}(i8*)** "
    writeLocal evalPtr ""
    evalResult <- writeNewLocal "call fastcc {i2,i8*}(i8* "
    writeLocal evalParam ")"
    status <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal evalResult ",0"
    statusPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 1"
    writeCode " store i2 "
    writeLocal status ",i2* "
    writeLocal statusPtr ""
    nextValueRaw <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal evalResult ",1"
    nextValuePtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 2"
    writeCode " store i8* "
    writeLocal nextValueRaw ",i8** "
    writeLocal nextValuePtr ""
    nextValue <- writeNewLocal "bitcast i8* "
    writeLocal nextValueRaw " to "
    writeValueType "*"
    return (status,nextValue)

writeAddRef :: Local -> GenLLVM ()
writeAddRef value = do
    refCountPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 0"
    oldRefCount <- writeNewLocal "load i32,i32* "
    writeLocal refCountPtr ""
    newRefCount <- writeNewLocal "add i32 1,"
    writeLocal oldRefCount ""
    writeCode " store i32 "
    writeLocal newRefCount ",i32* "
    writeLocal refCountPtr ""

writeUnref :: Either Local String -> GenLLVM ()
writeUnref value = do
    writeCode " call fastcc void @unref("
    writeValueType "* "
    either (flip writeLocal "") writeCode value
    writeCode ")"

writeDecls :: GenLLVM ()
writeDecls = do
    writeCode "declare i8* @malloc(i32)"
    writeCode "declare void @free(i8*)"
    writeCode "declare void @abort() noreturn"
    writeCode "declare i32 @read(i32,i8*,i32)"
    writeCode "declare i32 @write(i32,i8*,i32)"
    writeCode "declare i32 @open(i8*,i32)"
    writeCode "declare void @perror(i8*)"
    writeCode "declare void @exit(i32) noreturn"

writeUnrefDefn :: GenLLVM ()
writeUnrefDefn = do
    writeCode "define private fastcc void @unref("
    writeValueType "* %value) {"
    writeNewLabel
    refCountPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* %value,i32 0,i32 0"
    oldRefCount <- writeNewLocal "load i32,i32* "
    writeLocal refCountPtr ""
    newRefCount <- writeNewLocal "sub i32 "
    writeLocal oldRefCount ",1"
    cmp <- writeNewLocal "icmp ugt i32 "
    writeLocal newRefCount ",0"
    (aliveRef,deadRef) <- writeBranch cmp

    writeNewLabelBack [aliveRef]
    writeCode " store i32 "
    writeLocal newRefCount ",i32* "
    writeLocal refCountPtr ""
    writeCode " ret void"

    writeNewLabelBack [deadRef]
    statusPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* %value,i32 0,i32 1"
    status <- writeNewLocal "load i2,i2* "
    writeLocal statusPtr ""
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status ""
    (unevaluatedRef,evaluatedRef) <- writeBranch cmp

    writeNewLabelBack [unevaluatedRef]
    evalParamPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* %value,i32 0,i32 2"
    evalParam <- writeNewLocal "load i8*,i8** "
    writeLocal evalParamPtr ""
    freeEvalParamPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* %value,i32 0,i32 4"
    freeEvalParam <- writeNewLocal "load void(i8*)*,void(i8*)** "
    writeLocal freeEvalParamPtr ""
    writeCode " call fastcc void "
    writeLocal freeEvalParam "(i8* "
    writeLocal evalParam ")"
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeCode " call void @free(i8* "
    writeLocal rawPtr ")"
    writeCode " ret void"

    writeNewLabelBack [evaluatedRef]
    cmp <- writeNewLocal "icmp eq i2 2,"
    writeLocal status ""
    (nilRef,nonnilRef) <- writeBranch cmp

    writeNewLabelBack [nilRef]
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeCode " call void @free("
    writeLocal rawPtr ")"
    writeCode " ret void"

    writeNewLabelBack [nonnilRef]
    nextValueRawPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* %value,i32 0,i32 2"
    nextValue <- writeNewLocal "bitcast i8* "
    writeLocal nextValueRawPtr " to "
    writeValueType "*"
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeCode " call void @free("
    writeLocal rawPtr ")"
    writeCode " musttail call fastcc void @unref("
    writeValueType "* "
    writeLocal nextValue ")"
    writeCode " ret void"
    writeCode " }"

writeAllocateNewValue :: Int -> GenLLVM Local
writeAllocateNewValue initialStatus = do
    ptrForSize <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* null,i32 1"
    size <- writeNewLocal "ptrtoint "
    writeValueType "* "
    writeLocal ptrForSize " to i32"
    rawPtr <- writeNewLocal "call i8* @malloc(i32 "
    writeLocal size ")"
    value <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeValueType "*"
    refCountPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 0"
    writeCode " store i32 1,i32 *"
    writeLocal refCountPtr ""
    statusPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 1"
    writeCode (" store i2 " ++ show initialStatus ++ ",i2 *")
    writeLocal statusPtr ""
    return value

writeLiteralValueEvalParamType :: String -> GenLLVM ()
writeLiteralValueEvalParamType code = do
    -- 0:[0 x i1]* bit array
    -- 1:i32 current index
    -- 2:i32 array length
    writeCode ("{[0 x i1]*,i32,i32}" ++ code)

writeNewConstantLiteralValue :: [Bool] -> GenLLVM Local
writeNewConstantLiteralValue bits = do
    bitArray <-
        writeNewLocal ("bitcast " ++ literalType bits ++ " "
                                  ++ literalName bits ++ " to [0 x i1]*")
    writeNewLiteralValue (Right 0) (Right (length bits)) bitArray

writeNewLiteralValue :: Either Local Int -> Either Local Int -> Local
                     -> GenLLVM Local
writeNewLiteralValue currentIndex arraySize bitArray = do
    value <- writeAllocateNewValue 3
    sizePtr <- writeNewLocal "getelementptr "
    writeLiteralValueEvalParamType ","
    writeLiteralValueEvalParamType "* null,i32 1"
    size <- writeNewLocal "ptrtoint "
    writeLiteralValueEvalParamType "* "
    writeLocal sizePtr " to i32"
    rawPtr <- writeNewLocal "call i8* @malloc(i32 "
    writeLocal size ")"
    evalParamRawPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 2"
    writeCode " store i8* "
    writeLocal rawPtr ",i8** "
    writeLocal evalParamRawPtr ""
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal evalParamRawPtr " to "
    writeLiteralValueEvalParamType "*"
    bitArrayPtr <- writeNewLocal "getelementptr "
    writeLiteralValueEvalParamType ","
    writeLiteralValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    writeCode " store [0 x i1]* "
    writeLocal bitArray ",[0 x i1]** "
    writeLocal bitArrayPtr ""
    currentIndexPtr <- writeNewLocal "getelementptr "
    writeLiteralValueEvalParamType ","
    writeLiteralValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1"
    writeCode " store i32 "
    either (flip writeLocal "") (writeCode . show) arraySize
    writeCode ",i32* "
    writeLocal currentIndexPtr ""
    arraySizePtr <- writeNewLocal "getelementptr "
    writeLiteralValueEvalParamType ","
    writeLiteralValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1"
    writeCode " store i32 "
    either (flip writeLocal "") (writeCode . show) arraySize
    writeCode ",i32* "
    writeLocal arraySizePtr ""
    evalFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 3"
    writeCode " store {i2,i8*}(i8*)* @evalLiteral,{i2,i8*}(i8*)** "
    writeLocal evalFuncPtr ""
    freeEvalParamFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 4"
    writeCode " store void(i8*)* @freeEvalParamLiteral,void(i8*)** "
    writeLocal freeEvalParamFuncPtr ""
    return value

writeEvalLiteralValueDefn :: GenLLVM ()
writeEvalLiteralValueDefn = do
    writeCode "define private fastcc {i2,i8*} @evalLiteral(i8* %evalParam) {"
    undefined
    writeCode " }"

writeFreeEvalParamLiteralValueDefn :: GenLLVM ()
writeFreeEvalParamLiteralValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamLiteral(i8* %evalParam) {"
    writeCode " call void @free(i8* %evalParam)"
    writeCode " ret void"
    writeCode " }"

writeConcatValueEvalParamType :: String -> GenLLVM ()
writeConcatValueEvalParamType code = do
    -- 0:value type* first
    -- 1:value type* rest
    writeCode "{"
    writeValueType "*,"
    writeValueType ("*}" ++ code)

writeNewConcatValue :: Local -> Local -> GenLLVM Local
writeNewConcatValue value1 value2 = do
    value <- writeAllocateNewValue 3
    sizePtr <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* null,i32 1"
    size <- writeNewLocal "ptrtoint "
    writeConcatValueEvalParamType "* "
    writeLocal sizePtr " to i32"
    rawPtr <- writeNewLocal "call i8* @malloc(i32 "
    writeLocal size ")"
    evalParamRawPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 2"
    writeCode " store i8* "
    writeLocal rawPtr ",i8** "
    writeLocal evalParamRawPtr ""
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal evalParamRawPtr " to "
    writeConcatValueEvalParamType "*"
    ptr1 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    writeCode " store "
    writeValueType "* "
    writeLocal value1 ","
    writeValueType "** "
    writeLocal ptr1 ""
    ptr2 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1"
    writeCode " store "
    writeValueType "* "
    writeLocal value2 ","
    writeValueType "** "
    writeLocal ptr2 ""
    evalFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 3"
    writeCode " store {i2,i8*}(i8*)* @evalConcat,{i2,i8*}(i8*)** "
    writeLocal evalFuncPtr ""
    freeEvalParamFuncPtr <- writeNewLocal "getelementptr "
    writeValueType ","
    writeValueType "* "
    writeLocal value ",i32 0,i32 4"
    writeCode " store void(i8*)* @freeEvalParamConcat,void(i8*)** "
    writeLocal freeEvalParamFuncPtr ""
    return value

writeEvalConcatValueDefn :: GenLLVM ()
writeEvalConcatValueDefn = do
    writeCode "define private fastcc {i2,i8*} @evalConcat(i8* %evalParam) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeConcatValueEvalParamType "*"
    ptr1 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    value1 <- writeNewLocal "load "
    writeValueType "*,"
    writeValueType "** "
    writeLocal ptr1 ""
    ptr2 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1"
    value2 <- writeNewLocal "load "
    writeValueType "*,"
    writeValueType "** "
    writeLocal ptr2 ""

    (status1,nextValue1) <- writeForceEval value1
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status1 ""
    (abortLabelRef1,okLabelRef1) <- writeBranch cmp

    abortLabel <- writeNewLabelBack [abortLabelRef1]
    writeCode " call void @abort() noreturn"

    writeNewLabelBack [okLabelRef1]
    cmp <- writeNewLocal "icmp ne i2 2,"
    writeLocal status1 ""
    (nonnilLabelRef1,nilLabelRef1) <- writeBranch cmp

    writeNewLabelBack [nonnilLabelRef1]
    writeAddRef nextValue1
    writeUnref (Left value1)
    newNext <- writeNewConcatValue nextValue1 value2
    newNextRawPtr <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal newNext " to i8*"
    retValue1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal status1 ",0"
    retValue <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retValue1 ",i8* "
    writeLocal newNextRawPtr ""
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""

    writeNewLabelBack [nilLabelRef1]
    writeUnref (Left value1)
    (status2,nextValue2) <- writeForceEval value2
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status2 ""
    (abortLabelRef2,okLabelRef2) <- writeBranch cmp
    abortLabelRef2 abortLabel

    writeNewLabelBack [okLabelRef2]
    cmp <- writeNewLocal "icmp ne i2 2,"
    writeLocal status2 ""
    (nonnilLabelRef2,nilLabelRef2) <- writeBranch cmp

    writeNewLabelBack [nonnilLabelRef2]
    writeAddRef nextValue2
    writeUnref (Left value2)
    newNextRawPtr2 <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal nextValue2 " to i8*"
    retValue1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal status2 ",0"
    retValue <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retValue ",i8* "
    writeLocal newNextRawPtr ""
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""

    writeNewLabelBack [nilLabelRef2]
    writeUnref (Left value2)
    writeCode " call void @free(i8* %evalParam)"
    retValue <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal status2 ",0"
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""
    writeCode " }"

writeFreeEvalParamConcatValueDefn :: GenLLVM ()
writeFreeEvalParamConcatValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamConcat(i8* %evalParam) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeConcatValueEvalParamType "*"
    ptr1 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 0"
    value1 <- writeNewLocal "load "
    writeValueType "*,"
    writeValueType "** "
    writeLocal ptr1 ""
    writeUnref (Left value1)
    ptr2 <- writeNewLocal "getelementptr "
    writeConcatValueEvalParamType ","
    writeConcatValueEvalParamType "* "
    writeLocal evalParam ",i32 0,i32 1"
    value2 <- writeNewLocal "load "
    writeValueType "*,"
    writeValueType "** "
    writeLocal ptr2 ""
    writeUnref (Left value2)
    writeCode " call void @free(i8* %evalParam)"
    writeCode " ret void"
    writeCode " }"

writeFileValueEvalParamType :: String -> GenLLVM ()
writeFileValueEvalParamType code = do
    -- 0:i32 file descriptor
    -- 1:i8 current bit index
    -- 2:i8 current byte
    writeCode ("{i32,i8,i8}" ++ code)

writeNewFileValue :: Either Local String -> GenLLVM Local
writeNewFileValue fd = do
    value <- writeAllocateNewValue 3
    undefined

writeEvalFileValueDefn :: GenLLVM ()
writeEvalFileValueDefn = do
    undefined

writeFreeEvalParamFileValueDefn :: GenLLVM ()
writeFreeEvalParamFileValueDefn = do
    undefined

writeNewNilValue :: GenLLVM Local
writeNewNilValue = writeAllocateNewValue 2

genMain :: String -> [Def] -> String
genMain name (Def params _:_) = genLLVM (do
    writeCode "define void @main(i32 %argc,i8** %argv) {"
    writeNewLabel
    nilValue <- writeNewNilValue
    stdinValue <- writeNewFileValue (Right "0")
    args <- mapM (writeArg nilValue stdinValue) [1 .. length params]
    writeUnref (Left nilValue)
    writeUnref (Left stdinValue)
    result <- writeNewLocal "call fastcc "
    writeValueType "* "
    writeName name
    writeCode "("
    zipWithM_ (\ comma arg -> do
            writeCode comma
            writeValueType "* "
            writeLocal arg "")
        ("":repeat ",") args
    writeCode " br label "
    entryLabelRef <- writeForwardRefLabel

    entryLabel <- writeNewLabelBack [entryLabelRef]
    buffer <- writeNewLocal "alloca i8,i32 1"
    writeCode " br label "
    loopLabelRef <- writeForwardRefLabel

    loopLabel <- writeNewLabelBack [loopLabelRef]
    (value,valuePhiRef) <-
        writePhi (writeValueType "*") (Right result) entryLabel
    (bitIndex,bitIndexPhiRef) <-
        writePhi (writeCode "i8") (Left "0") entryLabel
    (byte,bytePhiRef) <- writePhi (writeCode "i8") (Left "0") entryLabel

    (status,nextValue) <- writeForceEval value
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status ""
    (abortLabelRef,okLabelRef) <- writeBranch cmp

    writeNewLabelBack [abortLabelRef]
    writeCode " call void @abort() noreturn"

    writeNewLabelBack [okLabelRef]
    cmp <- writeNewLocal "icmp eq i2 2,"
    writeLocal status ""
    (eofLabelRef,nextBitLabelRef) <- writeBranch cmp

    writeNewLabelBack [eofLabelRef]
    writeCode " call void @exit(i32 0) noreturn"

    nextBitLabel <- writeNewLabelBack [nextBitLabelRef]
    nextBit <- writeNewLocal "zext i2 "
    writeLocal status " to i8"
    shiftedBit <- writeNewLocal "shl i8 "
    writeLocal nextBit ","
    writeLocal bitIndex ""
    nextByte <- writeNewLocal "or i8 "
    writeLocal byte ","
    writeLocal shiftedBit ""
    nextBitIndex <- writeNewLocal "add i8 1,"
    writeLocal bitIndex ""
    valuePhiRef nextValue nextBitLabel
    bitIndexPhiRef nextBitIndex nextBitLabel
    bytePhiRef nextByte nextBitLabel
    cmp <- writeNewLocal "icmp ult i8 "
    writeLocal nextBitIndex ",8"
    (continueRef,nextByteRef) <- writeBranch cmp
    continueRef loopLabel

    nextByteLabel <- writeNewLabelBack [nextByteRef]
    writeCode " store i8 "
    writeLocal nextByte ",i8* "
    writeLocal buffer ""
    writeNewLocal " call i32 @write(i32 1,i8* "
    writeLocal buffer ",i32 1)"
    zero <- writeNewLocal "select i1 1,i8 0,i8 0"
    valuePhiRef nextValue nextByteLabel
    bitIndexPhiRef zero nextByteLabel
    bytePhiRef zero nextByteLabel
    writeCode " br label "
    writeLabelRef loopLabel ""
    writeCode " }")
  where
    writeArg nilValue stdinValue index = do
        -- if index < argc, open(argv[index])
        -- else if index == argc, stdin
        -- else nil
        cmp <- writeNewLocal ("icmp ult i32 " ++ show index ++ ",%argc")
        (fileArgLabelRef,unspecifiedArgLabelRef) <- writeBranch cmp

        writeNewLabelBack [fileArgLabelRef]
        filenamePtr <-
            writeNewLocal ("getelementptr i8*,i8** %argv,i32 "++ show index)
        filename <- writeNewLocal "load i8*,i8** "
        writeLocal filenamePtr ""
        fd <- writeNewLocal "call @open(i8* "
        writeLocal filename ",i32 0)"
        cmp <- writeNewLocal "icmp sge i32 "
        writeLocal fd ",0"
        (openFileSuccessLabelRef,openFileFailLabelRef) <- writeBranch cmp

        openFileSuccessLabel <- writeNewLabelBack [openFileSuccessLabelRef]
        fileValue <- writeNewFileValue (Left fd)
        writeCode " br label "
        endArgLabelRef1 <- writeForwardRefLabel

        writeNewLabelBack [openFileFailLabelRef]
        writeCode " call void @perror(i8* "
        writeLocal filename ")"
        writeCode " call void @exit(i32 -1) noreturn"

        writeNewLabelBack [unspecifiedArgLabelRef]
        cmp <- writeNewLocal ("icmp eq i32 " ++ show index ++ ",%argc")
        (stdinArgLabelRef,nilArgLabelRef) <- writeBranch cmp

        stdinArgLabel <- writeNewLabelBack [stdinArgLabelRef]
        writeAddRef stdinValue
        writeCode " br label "
        endArgLabelRef2 <- writeForwardRefLabel

        nilArgLabel <- writeNewLabelBack [nilArgLabelRef]
        writeAddRef nilValue
        writeCode " br label "
        endArgLabelRef3 <- writeForwardRefLabel

        writeNewLabelBack [endArgLabelRef1,endArgLabelRef2,endArgLabelRef3]
        argValue <- writeNewLocal "phi "
        writeValueType "* ["
        writeLocal fileValue ","
        writeLabelRef openFileSuccessLabel "],["
        writeLocal stdinValue ","
        writeLabelRef stdinArgLabel "],["
        writeLocal nilValue ","
        writeLabelRef nilArgLabel "]"
        return argValue
