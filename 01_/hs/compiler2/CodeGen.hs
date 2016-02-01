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
     writeCode,writeLocal,writeLabel,writeLabelRef,
     newLocal,newLabel,writeNewLocal,writeNewLabel,writeNewLabelBack,
     forwardRefLabel,writeForwardRefLabel,writeBranch,writePhi)

codeGen :: [Func] -> String
codeGen funcs =
    (concatMap writeLiteral . Set.toList . foldl collectLiterals Set.empty)
            funcs
        ++ concatMap genLLVM [ -- runtime functions
            writeDecls,
            writeUnrefDefn,
            writeLiteralValueDefns,
            writeConcatValueDefns,
            writeFileValueDefns
            ]
        ++ (concatMap genLLVM . map writeFunc) funcs

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
writeName name = writeCode ("@_" ++ concatMap quote name)
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
writeFunc (name,defs) = do
    writeCode "define fastcc "
    writeValueType "* "
    writeName name
    writeCode "("
    let Def params _:_ = defs
    zipWithM_ (\ comma i -> do
            writeCode comma
            writeValueType ("* %a" ++ show i))
        ("":repeat ",") [0 .. length params - 1]
    writeCode ") {"
    writeNewLabel

    writeCode " br label "
    labelRef <- writeForwardRefLabel
    labelRefs <- foldM (\ labelRefs def -> do
            writeNewLabelBack labelRefs
            writeDef def)
        [labelRef] defs
    unless (null labelRefs) (do
        writeNewLabelBack labelRefs
        writeCode " call void @abort() noreturn")
    writeCode " }"

writeDef :: Def -> GenLLVM [Label -> GenLLVM ()]
writeDef (Def params expr) = do
    (bindings,_,nextDefLabelRefs) <- foldM writeBindParam ([],0,[]) params
    mapM_ writeAddRef bindings
    mapM_ (writeUnref . Right . ("%a" ++) . show) [0 .. length params - 1]
    value <- writeBody bindings expr
    writeCode " ret "
    writeValueType "* "
    writeLocal value ""
    writeCode " }"
    return nextDefLabelRefs

writeBindParam :: ([Local],Int,[Label -> GenLLVM ()]) -> Param
               -> GenLLVM ([Local],Int,[Label -> GenLLVM ()])
writeBindParam (bindings,index,nextDefLabelRefs) param = w param
  where
    w (ParamBound _ bits _) = do
        (value,labelRefs) <- writeCheckBits bits
        return (bindings ++ [value],index+1,labelRefs ++ nextDefLabelRefs)
    w (ParamIgnored _ bits) = do
        (value,labelRefs) <- writeCheckBits bits
        return (bindings,index+1,labelRefs ++ nextDefLabelRefs)
    w (ParamLiteral _ bits) = do
        (value,labelRefs) <- writeCheckBits bits
        (valueState,_) <- writeForceEval value
        cmp <- writeNewLocal "icmp ne i2 2,"
        writeLocal valueState ""
        (tryNextDefRef,bindSuccessRef) <- writeBranch cmp
        writeNewLabelBack [bindSuccessRef]
        return (bindings,index+1,tryNextDefRef : labelRefs ++ nextDefLabelRefs)
    writeCheckBits bits = do
        value <- writeNewLocal "select i1 1,"
        writeValueType ("* %a" ++ show index ++ ",")
        writeValueType "* null"
        foldM writeCheckBit (value,[]) bits
    writeCheckBit (value,labelRefs) bit = do
        (valueState,nextValue) <- writeForceEval value
        cmp <- writeNewLocal "icmp eq i2 "
        writeLocal valueState (if bit then ",1" else ",0")
        (matchRef,notMatchRef) <- writeBranch cmp
        writeNewLabelBack [matchRef]
        return (nextValue,notMatchRef:labelRefs)

writeBody :: [Local] -> Expr -> GenLLVM Local
writeBody bindings expr = w expr
  where
    w (ExprFuncall (Identifier _ name) exprs) = do
        args <- mapM (writeExpr bindings) exprs
        mapM_ (writeUnref . Left) bindings
        value <- writeNewLocal "musttail call fastcc "
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
    w expr = do
        value <- writeExpr bindings expr
        writeAddRef value
        mapM_ (writeUnref . Left) bindings
        return value

writeExpr :: [Local] -> Expr -> GenLLVM Local
writeExpr bindings expr = w expr
  where
    w (ExprLiteral _ bits) = do
        writeNewLiteralValue bits
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

writeNewLiteralValue :: [Bool] -> GenLLVM Local
writeNewLiteralValue bits = do
    undefined

writeLiteralValueDefns :: GenLLVM ()
writeLiteralValueDefns = do
    undefined

writeNewConcatValue :: Local -> Local -> GenLLVM Local
writeNewConcatValue value1 value2 = do
    undefined

writeConcatValueDefns :: GenLLVM ()
writeConcatValueDefns = do
    undefined

writeNewFileValue :: Either Local String -> GenLLVM Local
writeNewFileValue fd = do
    undefined

writeFileValueDefns :: GenLLVM ()
writeFileValueDefns = do
    undefined

writeNewNilValue :: GenLLVM Local
writeNewNilValue = do
    undefined

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
        undefined
