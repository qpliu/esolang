module CodeGen
    (codeGen,genMain)
where

import Control.Monad(foldM,unless,when,zipWithM_)
import Data.Char(isAlphaNum,isAscii,ord)
import Data.Set(Set)
import qualified Data.Set as Set

import Ast(Identifier(..),Param(..),Def(..),Expr(..),Func)
import GenLLVM
    (GenLLVM,Local,Label,genLLVM,
     writeCode,writeLocal,writeLabelRef,
     writeNewLocal,writeNewLabel,writeNewLabelBack,
     writeForwardRefLabel,writeBranch,writePhi,
     writeGetElementPtr,writeLoad,writeStore)

debugMemory :: Bool
debugMemory = False

debugMemoryAllocs :: Bool
debugMemoryAllocs = False

debugAborts :: Bool
debugAborts = False

debugIndirectCalls :: Bool
debugIndirectCalls = False

debugDeref :: Bool
debugDeref = False

-- Generate code for LLVM 3.7
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
            writeFreeEvalParamFuncValueDefn,
            writeFreeEvalParamNullaryFuncValueDefn,
            writeDebugDefns,
            writeDebugMemoryMallocDefn,
            writeDebugMemoryFreeDefn
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
    collectExprLiterals literals _ = literals

writeLiteral :: [Bool] -> String
writeLiteral bits | null bits = "" | otherwise =
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

evalFuncName :: String -> String
evalFuncName name = "@evalFunc_" ++ quoteName name

quoteName :: String -> String
quoteName name = concatMap quote name
  where
    quote c | isAscii c && isAlphaNum c = [c]
            | otherwise = "_" ++ show (ord c) ++ "_"

quoteStringConstant :: String -> String
quoteStringConstant s = concatMap quote s
  where
    quote '\\' = "\\\\"
    quote '"' = "\\\""
    quote c = [c]

writeValueType :: String -> GenLLVM ()
writeValueType code =
    -- 0:i32 ref count
    -- 1:i2 status (0:0,1:1,2:nil,3:unevaluated)
    -- 2:i8* next/eval param
    -- 3:{i2,i8*}(i8*,i8*)* eval(eval param,value)
    -- 4:void(i8*)* free(eval param)
    writeCode ("{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}" ++ code)

writeValueFieldPtr :: Local -> Int -> GenLLVM Local
writeValueFieldPtr value field =
    writeGetElementPtr (writeValueType "")
                       (Left value) ("i32 0,i32 " ++ show field)

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
    (value,_) <- writeAllocateNewValue 3
    if null params
        then return ()
        else do
            sizePtr <- writeGetElementPtr (writeFuncValueEvalParamType "")
                                          (Right "null")
                                          ("i32 0,i32 1,i32 " ++
                                           show (length params))
            size <- writeNewLocal "ptrtoint "
            writeValueType "** "
            writeLocal sizePtr " to i32"
            rawPtr <- writeMalloc size
            evalParam <- writeNewLocal "bitcast i8* "
            writeLocal rawPtr " to "
            writeFuncValueEvalParamType "*"
            argCountPtr <- writeGetElementPtr (writeFuncValueEvalParamType "")
                                              (Left evalParam) "i32 0,i32 0"
            writeStore
                (writeCode "i32") (Right (show (length params))) argCountPtr
            mapM_ (\ i -> do
                    argPtr <-
                        writeGetElementPtr (writeFuncValueEvalParamType "")
                                           (Left evalParam)
                                           ("i32 0,i32 1,i32 " ++ show i)
                    writeStore
                        (writeValueType "*") (Right ("%a" ++ show i)) argPtr)
                [0 .. length params - 1]

            evalParamPtr <- writeValueFieldPtr value 2
            writeStore (writeCode "i8*") (Left rawPtr) evalParamPtr

    evalFuncPtr <- writeValueFieldPtr value 3
    writeStore (writeCode "{i2,i8*}(i8*,i8*)*")
               (Right (evalFuncName name)) evalFuncPtr

    freeEvalParamFuncPtr <- writeValueFieldPtr value 4
    writeStore (writeCode "void(i8*)*")
               (Right (if null params
                        then "@freeEvalParamNullaryFunc"
                        else "@freeEvalParamFunc"))
               freeEvalParamFuncPtr

    writeCode " ret "
    writeValueType "* "
    writeLocal value ""
    writeCode " }"

writeFuncValueEvalParamType :: String -> GenLLVM ()
writeFuncValueEvalParamType code = do
    -- 0:i32 number of arguments
    -- 1:[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}] arguments
    writeCode "{i32,[0 x "
    writeValueType ("*]}" ++ code)

writeFreeEvalParamFuncValueDefn :: GenLLVM ()
writeFreeEvalParamFuncValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamFunc(i8* %evalParam) {"
    entryLabel <- writeNewLabel
    writeDebugDeref "FPF" "%evalParam"
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeFuncValueEvalParamType "*"
    argCountPtr <- writeGetElementPtr (writeFuncValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 0"
    argCount <- writeLoad (writeCode "i32") argCountPtr
    writeCode " br label "
    loopLabelRef <- writeForwardRefLabel

    loopLabel <- writeNewLabelBack [loopLabelRef]
    (argIndex,argIndexPhiRef) <-
        writePhi (writeCode "i32") (Right "0") entryLabel
    cmp <- writeNewLocal "icmp ult i32 "
    writeLocal argIndex ","
    writeLocal argCount ""
    (continueLoopLabelRef,doneLabelRef) <- writeBranch cmp

    continueLoopLabel <- writeNewLabelBack [continueLoopLabelRef]
    argPtr <- writeGetElementPtr (writeFuncValueEvalParamType "")
                                 (Left evalParam) "i32 0,i32 1,i32 "
    writeLocal argIndex ""
    arg <- writeLoad (writeValueType "*") argPtr
    writeUnref (Left arg)
    newArgIndex <- writeNewLocal "add i32 1,"
    writeLocal argIndex ""
    argIndexPhiRef newArgIndex continueLoopLabel
    writeCode " br label "
    writeLabelRef loopLabel ""

    writeNewLabelBack [doneLabelRef]
    writeFree (Right "%evalParam")
    writeCode " ret void"
    writeCode " }"

writeFreeEvalParamNullaryFuncValueDefn :: GenLLVM ()
writeFreeEvalParamNullaryFuncValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamNullaryFunc(i8* %evalParam) {"
    writeCode " ret void"
    writeCode " }"

writeEvalFuncValue :: Func -> GenLLVM ()
writeEvalFuncValue (name,defs) = do
    writeCode "define private fastcc {i2,i8*} "
    writeCode (evalFuncName name)
    writeCode "(i8* %evalParam,i8* %value) {"
    writeNewLabel
    value <- writeNewLocal "bitcast i8* %value to "
    writeValueType "*"

    let Def params _:_ = defs
    args <- if null params
        then return []
        else do
            writeDebugDeref "EFP" "%evalParam"
            evalParam <- writeNewLocal "bitcast i8* %evalParam to "
            writeFuncValueEvalParamType "*"
            args <- mapM (\ i -> do
                    argPtr <-
                        writeGetElementPtr (writeFuncValueEvalParamType "")
                                           (Left evalParam)
                                           ("i32 0,i32 1,i32 " ++ show i)
                    writeLoad (writeValueType "*") argPtr)
                [0 .. length params - 1]
            writeFree (Right "%evalParam")
            return args

    writeCode " br label "
    labelRef <- writeForwardRefLabel
    labelRefs <- foldM (\ labelRefs def -> do
            writeNewLabelBack labelRefs
            writeDef def value args)
        [labelRef] defs
    unless (null labelRefs) (do
        writeNewLabelBack labelRefs
        when debugAborts (do
            writeNewLocal ("call i32 @write(i32 1,i8* getelementptr (["
                           ++ show (length name) ++ " x i8],["
                           ++ show (length name) ++ " x i8]* @name_"
                           ++ quoteName name ++ ",i32 0,i32 0),i32 "
                           ++ show (length name) ++ ")")
            return ())
        writeCode " call void @abort() noreturn"
        writeCode " ret {i2,i8*} undef")
    writeCode " }"
    when debugAborts (do
        writeCode ("@name_" ++ quoteName name ++ " = private constant ["
                            ++ show (length name) ++ " x i8] c\""
                            ++ quoteStringConstant name ++ "\""))

writeDef :: Def -> Local -> [Local] -> GenLLVM [Label -> GenLLVM ()]
writeDef (Def params expr) value args = do
    (bindings,nextDefLabelRefs) <-
        foldM writeBindParam ([],[]) (zip args params)
    mapM_ writeAddRef bindings
    mapM_ (writeUnref . Left) args
    writeDefExpr value bindings expr
    return nextDefLabelRefs

writeDefExpr :: Local -> [Local] -> Expr -> GenLLVM ()
writeDefExpr value bindings expr = w expr
  where
    w (ExprLiteral _ []) = do
        mapM_ (writeUnref . Left) bindings
        statusPtr <- writeValueFieldPtr value 1
        writeStore (writeCode "i2") (Right "2") statusPtr
        retValue (Right "2") Nothing
    w (ExprLiteral _ bits) = do
        mapM_ (writeUnref . Left) bindings
        evalParamRawPtr <- writeNewLiteralValueEvalParam bits
        tailCallForceRetValue evalParamRawPtr (Right "@evalLiteral")
    w (ExprBound index) = do
        let rhs = bindings !! index
        mapM_ (writeUnref . Left)
              (take index bindings ++ drop (index+1) bindings)
        rhsStatusPtr <- writeValueFieldPtr rhs 1
        rhsStatus <- writeLoad (writeCode "i2") rhsStatusPtr
        cmp <- writeNewLocal "icmp ne i2 3,"
        writeLocal rhsStatus ""
        (evaluatedLabelRef,unevaluatedLabelRef) <- writeBranch cmp

        writeNewLabelBack [evaluatedLabelRef]
        valueStatusPtr <- writeValueFieldPtr value 1
        writeStore (writeCode "i2") (Left rhsStatus) valueStatusPtr
        cmp <- writeNewLocal "icmp eq i2 2,"
        writeLocal rhsStatus ""
        (nilLabelRef,nonnilLabelRef) <- writeBranch cmp

        writeNewLabelBack [nilLabelRef]
        writeUnref (Left rhs)
        retValue (Right "2") Nothing

        writeNewLabelBack [nonnilLabelRef]
        rhsNextPtrPtr <- writeValueFieldPtr rhs 2
        rhsNextRawPtr <- writeLoad (writeCode "i8*") rhsNextPtrPtr
        valueNextPtrPtr <- writeValueFieldPtr value 2
        writeStore (writeCode "i8*") (Left rhsNextRawPtr) valueNextPtrPtr
        valueNext <- writeNewLocal "bitcast i8* "
        writeLocal rhsNextRawPtr " to "
        writeValueType "*"
        writeAddRef valueNext
        writeUnref (Left rhs)
        retValue (Left rhsStatus) (Just rhsNextRawPtr)

        writeNewLabelBack [unevaluatedLabelRef]
        rhsEvalParamPtr <- writeValueFieldPtr rhs 2
        rhsEvalParam <- writeLoad (writeCode "i8*") rhsEvalParamPtr
        rhsEvalFuncPtr <- writeValueFieldPtr rhs 3
        rhsEvalFunc <-
            writeLoad (writeCode "{i2,i8*}(i8*,i8*)*") rhsEvalFuncPtr
        rhsRawPtr <- writeNewLocal "bitcast "
        writeValueType "* "
        writeLocal rhs " to i8*"
        refCountPtr <- writeValueFieldPtr rhs 0
        refCount <- writeLoad (writeCode "i32") refCountPtr
        cmp <- writeNewLocal "icmp ule i32 "
        writeLocal refCount ",1"
        (noOtherRefsLabelRef,hasOtherRefsLabelRef) <- writeBranch cmp

        writeNewLabelBack [noOtherRefsLabelRef]
        writeFree (Left rhsRawPtr)
        tailCallForceRetValue rhsEvalParam (Left rhsEvalFunc)

        writeNewLabelBack [hasOtherRefsLabelRef]
        when debugIndirectCalls (do
            writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                           ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                           ++ ",i32 0,i32 0),i8 69,{i2,i8*}(i8*,i8*)* ")
            writeLocal rhsEvalFunc ")"
            writeNewLocal "call i32 @fflush(i8* null)"
            return ())
        forcedResult <- writeNewLocal "call fastcc {i2,i8*} "
        writeLocal rhsEvalFunc "(i8* "
        writeLocal rhsEvalParam ",i8* "
        writeLocal rhsRawPtr ")"
        forcedStatus <- writeNewLocal "extractvalue {i2,i8*} "
        writeLocal forcedResult ",0"
        forcedNext <- writeNewLocal "extractvalue {i2,i8*} "
        writeLocal forcedResult ",1"
        nextValue <- writeNewLocal "bitcast i8* "
        writeLocal forcedNext " to "
        writeValueType "*"
        writeAddRef nextValue
        writeUnref (Left rhs)
        valueStatusPtr <- writeValueFieldPtr value 1
        writeStore (writeCode "i2") (Left forcedStatus) valueStatusPtr
        valueNextPtrPtr <- writeValueFieldPtr value 2
        writeStore (writeCode "i8*") (Left forcedNext) valueNextPtrPtr
        writeCode " ret {i2,i8*} "
        writeLocal forcedResult ""
    w (ExprFuncall (Identifier _ name) exprs) = do
        rhs <- writeExpr bindings expr
        mapM_ (writeUnref . Left) bindings
        -- rhs.refcount must be 1, so copy rhs into value
        -- future optimization: rewrite function def to take a value to
        -- write into instead of allocating and returning a new value
        rhsEvalParamPtr <- writeValueFieldPtr rhs 2
        rhsEvalParam <- writeLoad (writeCode "i8*") rhsEvalParamPtr
        rhsEvalFuncPtr <- writeValueFieldPtr rhs 3
        rhsEvalFunc <-
            writeLoad (writeCode "{i2,i8*}(i8*,i8*)*") rhsEvalFuncPtr
        rhsRawPtr <- writeNewLocal "bitcast "
        writeValueType "* "
        writeLocal rhs " to i8*"
        writeFree (Left rhsRawPtr)
        tailCallForceRetValue rhsEvalParam (Left rhsEvalFunc)
    w (ExprConcat expr1 expr2) = do
        value1 <- writeExpr bindings expr1
        value2 <- writeExpr bindings expr2
        mapM_ (writeUnref . Left) bindings
        evalParamRawPtr <- writeNewConcatValueEvalParam value1 value2
        tailCallForceRetValue evalParamRawPtr (Right "@evalConcat")
    retValue status nextValue = do
        retval1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
        either (flip writeLocal "") writeCode status
        writeCode ",0"
        retval <- maybe (return retval1) (\ nextValue -> do
                result <- writeNewLocal "insertvalue {i2,i8*} "
                writeLocal retval1 ",i8* "
                writeLocal nextValue ",1"
                return result)
            nextValue
        writeCode " ret {i2,i8*} "
        writeLocal retval ""
    tailCallForceRetValue evalParam evalFunc = do
        when debugIndirectCalls (do
            writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                           ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                           ++ ",i32 0,i32 0),i8 101,{i2,i8*}(i8*,i8*)* ")
            either (flip writeLocal "") writeCode evalFunc
            writeCode ")"
            writeNewLocal "call i32 @fflush(i8* null)"
            return ())
        retval <- writeNewLocal "musttail call fastcc {i2,i8*} "
        either (flip writeLocal "") writeCode evalFunc
        writeCode "(i8* "
        writeLocal evalParam ",i8* %value)"
        writeCode " ret {i2,i8*} "
        writeLocal retval ""

writeBindParam :: ([Local],[Label -> GenLLVM ()]) -> (Local,Param)
               -> GenLLVM ([Local],[Label -> GenLLVM ()])
writeBindParam (bindings,nextDefLabelRefs) (arg,param) = w param
  where
    w (ParamBound _ bits _) = do
        (value,nextDefLabelRefs2) <-
            foldM writeCheckBit (arg,nextDefLabelRefs) bits
        return (bindings ++ [value],nextDefLabelRefs2)
    w (ParamIgnored _ bits) = do
        (value,nextDefLabelRefs2) <-
            foldM writeCheckBit (arg,nextDefLabelRefs) bits
        return (bindings,nextDefLabelRefs2)
    w (ParamLiteral _ bits) = do
        (value,nextDefLabelRefs2) <-
            foldM writeCheckBit (arg,nextDefLabelRefs) bits
        (_,nextDefLabelRefs3) <-
            writeCheckForceEval value nextDefLabelRefs2 "2"
        return (bindings,nextDefLabelRefs3)
    writeCheckBit (value,nextDefLabelRefs) bit = do
        writeDebugDerefValue "EFb" value
        (nextRawPtr,nextDefLabelRefs2) <-
            writeCheckForceEval value nextDefLabelRefs
                                (if bit then "1" else "0")
        nextValue <- writeNewLocal "bitcast i8* "
        writeLocal nextRawPtr " to "
        writeValueType "*"
        return (nextValue,nextDefLabelRefs2)
    writeCheckForceEval value nextDefLabelRefs matchStatus = do
        writeCode " br label "
        checkForceLabelRef <- writeForwardRefLabel

        checkForceLabel <- writeNewLabelBack [checkForceLabelRef]
        writeDebugDerefValue "EF_" value
        statusPtr <- writeValueFieldPtr value 1
        status <- writeLoad (writeCode "i2") statusPtr
        evalParamPtr <- writeValueFieldPtr value 2
        evalParam <- writeLoad (writeCode "i8*") evalParamPtr
        cmp <- writeNewLocal "icmp eq i2 3,"
        writeLocal status ""
        (unevaluatedLabelRef,evaluatedLabelRef) <- writeBranch cmp

        unevaluatedLabel <- writeNewLabelBack [unevaluatedLabelRef]
        evalFuncPtr <- writeValueFieldPtr value 3
        evalFunc <- writeLoad (writeCode "{i2,i8*}(i8*,i8*)*") evalFuncPtr
        valueRawPtr <- writeNewLocal "bitcast "
        writeValueType "* "
        writeLocal value " to i8*"
        when debugIndirectCalls (do
            writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                           ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                           ++ ",i32 0,i32 0),i8 69,{i2,i8*}(i8*,i8*)* ")
            writeLocal evalFunc ")"
            writeNewLocal "call i32 @fflush(i8* null)"
            return ())
        forcedResult <- writeNewLocal "call fastcc {i2,i8*} "
        writeLocal evalFunc "(i8* "
        writeLocal evalParam ",i8* "
        writeLocal valueRawPtr ")"
        forcedStatus <- writeNewLocal "extractvalue {i2,i8*} "
        writeLocal forcedResult ",0"
        forcedNextRawPtr <- writeNewLocal "extractvalue {i2,i8*} "
        writeLocal forcedResult ",1"
        writeCode " br label "
        evaluatedLabelRef2 <- writeForwardRefLabel

        writeNewLabelBack [evaluatedLabelRef,evaluatedLabelRef2]
        status2 <- writeNewLocal "phi i2 ["
        writeLocal status ","
        writeLabelRef checkForceLabel "],["
        writeLocal forcedStatus ","
        writeLabelRef unevaluatedLabel "]"
        nextRawPtr2 <- writeNewLocal "phi i8* ["
        writeLocal evalParam ","
        writeLabelRef checkForceLabel "],["
        writeLocal forcedNextRawPtr ","
        writeLabelRef unevaluatedLabel "]"
        cmp <- writeNewLocal ("icmp eq i2 " ++ matchStatus ++ ",")
        writeLocal status2 ""
        (matchLabelRef,nextDefLabelRef) <- writeBranch cmp

        writeNewLabelBack [matchLabelRef]
        return (nextRawPtr2,nextDefLabelRef:nextDefLabelRefs)

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
        (value,_) <- writeNewConcatValue value1 value2
        return value

writeForceEval :: Local -> GenLLVM (Local,Local)
writeForceEval value = do
    statusPtr <- writeValueFieldPtr value 1
    status <- writeLoad (writeCode "i2") statusPtr
    cmp <- writeNewLocal "icmp ne i2 3,"
    writeLocal status ""
    (evaluatedLabelRef,unevaluatedLabelRef) <- writeBranch cmp

    evaluatedLabel <- writeNewLabelBack [evaluatedLabelRef]
    cmp <- writeNewLocal "icmp ne i2 2,"
    writeLocal status ""
    (nonnilLabelRef,doneLabelRef) <- writeBranch cmp

    nonnilLabel <- writeNewLabelBack [nonnilLabelRef]
    nextPtrPtr <- writeValueFieldPtr value 2
    nextRawPtr <- writeLoad (writeCode "i8*") nextPtrPtr
    next <- writeNewLocal "bitcast i8* "
    writeLocal nextRawPtr " to "
    writeValueType "*"
    writeCode " br label "
    doneLabelRef2 <- writeForwardRefLabel

    unevaluatedLabel <- writeNewLabelBack [unevaluatedLabelRef]
    (forcedStatus,forcedNext,_,_) <- writeForceEvalUnevaluated value
    writeCode " br label "
    doneLabelRef3 <- writeForwardRefLabel

    writeNewLabelBack [doneLabelRef,doneLabelRef2,doneLabelRef3]
    finalStatus <- writeNewLocal "phi i2 ["
    writeLocal status ","
    writeLabelRef evaluatedLabel "],["
    writeLocal status ","
    writeLabelRef nonnilLabel "],["
    writeLocal forcedStatus ","
    writeLabelRef unevaluatedLabel "]"
    finalNext <- writeNewLocal "phi "
    writeValueType "* [null,"
    writeLabelRef evaluatedLabel "],["
    writeLocal next ","
    writeLabelRef nonnilLabel "],["
    writeLocal forcedNext ","
    writeLabelRef unevaluatedLabel "]"
    return (finalStatus,finalNext)

writeForceEvalUnevaluated :: Local -> GenLLVM (Local,Local,Local,Local)
writeForceEvalUnevaluated value = do
    evalParamPtr <- writeValueFieldPtr value 2
    evalParam <- writeLoad (writeCode "i8*") evalParamPtr
    evalPtr <- writeValueFieldPtr value 3
    valueRawPtr <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal value " to i8*"
    eval <- writeLoad (writeCode "{i2,i8*}(i8*,i8*)*") evalPtr
    when debugIndirectCalls (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                       ++ ",i32 0,i32 0),i8 69,{i2,i8*}(i8*,i8*)* ")
        writeLocal eval ")"
        writeNewLocal "call i32 @fflush(i8* null)"
        return ())
    evalResult <- writeNewLocal "call fastcc {i2,i8*} "
    writeLocal eval "(i8* "
    writeLocal evalParam ",i8* "
    writeLocal valueRawPtr ")"
    status <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal evalResult ",0"
    nextValueRawPtr <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal evalResult ",1"
    nextValue <- writeNewLocal "bitcast i8* "
    writeLocal nextValueRawPtr " to "
    writeValueType "*"
    return (status,nextValue,nextValueRawPtr,evalResult)

writeAddRef :: Local -> GenLLVM ()
writeAddRef value = do
    refCountPtr <- writeValueFieldPtr value 0
    oldRefCount <- writeLoad (writeCode "i32") refCountPtr
    newRefCount <- writeNewLocal "add i32 1,"
    writeLocal oldRefCount ""
    writeStore (writeCode "i32") (Left newRefCount) refCountPtr

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
    writeCode "declare void @abort() noreturn "
    writeCode "declare i32 @read(i32,i8*,i32)"
    writeCode "declare i32 @write(i32,i8*,i32)"
    writeCode "declare i32 @open(i8*,i32)"
    writeCode "declare i32 @close(i32)"
    writeCode "declare void @perror(i8*)"
    writeCode "declare void @exit(i32) noreturn "

writeUnrefDefn :: GenLLVM ()
writeUnrefDefn = do
    writeCode "define private fastcc void @unref("
    writeValueType "* %value) {"
    writeNewLabel
    refCountPtr <- writeGetElementPtr (writeValueType "")
                                      (Right "%value") "i32 0,i32 0"
    oldRefCount <- writeLoad (writeCode "i32") refCountPtr
    newRefCount <- writeNewLocal "sub i32 "
    writeLocal oldRefCount ",1"
    cmp <- writeNewLocal "icmp ugt i32 "
    writeLocal newRefCount ",0"
    (aliveRef,deadRef) <- writeBranch cmp

    writeNewLabelBack [aliveRef]
    writeStore (writeCode "i32") (Left newRefCount) refCountPtr
    writeCode " ret void"

    writeNewLabelBack [deadRef]
    statusPtr <- writeGetElementPtr (writeValueType "")
                                    (Right "%value") "i32 0,i32 1"
    status <- writeLoad (writeCode "i2") statusPtr
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status ""
    (unevaluatedRef,evaluatedRef) <- writeBranch cmp

    writeNewLabelBack [unevaluatedRef]
    evalParamPtr <- writeGetElementPtr (writeValueType "")
                                       (Right "%value") "i32 0,i32 2"
    evalParam <- writeLoad (writeCode "i8*") evalParamPtr
    freeEvalParamPtr <- writeGetElementPtr (writeValueType "")
                                           (Right "%value") "i32 0,i32 4"
    freeEvalParam <- writeLoad (writeCode "void(i8*)*") freeEvalParamPtr
    when debugIndirectCalls (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                       ++ ",i32 0,i32 0),i8 70,void(i8*)* ")
        writeLocal freeEvalParam ")"
        writeNewLocal "call i32 @fflush(i8* null)"
        return ())
    writeCode " call fastcc void "
    writeLocal freeEvalParam "(i8* "
    writeLocal evalParam ")"
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeFree (Left rawPtr)
    writeCode " ret void"

    writeNewLabelBack [evaluatedRef]
    cmp <- writeNewLocal "icmp eq i2 2,"
    writeLocal status ""
    (nilRef,nonnilRef) <- writeBranch cmp

    writeNewLabelBack [nilRef]
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeFree (Left rawPtr)
    writeCode " ret void"

    writeNewLabelBack [nonnilRef]
    nextValueRawPtrPtr <- writeGetElementPtr (writeValueType "")
                                             (Right "%value") "i32 0,i32 2"
    nextValueRawPtr <- writeLoad (writeCode "i8*") nextValueRawPtrPtr
    nextValue <- writeNewLocal "bitcast i8* "
    writeLocal nextValueRawPtr " to "
    writeValueType "*"
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* %value to i8*"
    writeFree (Left rawPtr)
    writeCode " musttail call fastcc void @unref("
    writeValueType "* "
    writeLocal nextValue ")"
    writeCode " ret void"
    writeCode " }"

writeAllocateNewValue :: Int -> GenLLVM (Local,Local)
writeAllocateNewValue initialStatus = do
    rawPtr <- writeAlloc (writeValueType "")
    value <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeValueType "*"
    refCountPtr <- writeValueFieldPtr value 0
    writeStore (writeCode "i32") (Right "1") refCountPtr
    statusPtr <- writeValueFieldPtr value 1
    writeStore (writeCode "i2") (Right (show initialStatus)) statusPtr
    return (value,rawPtr)

writeLiteralValueEvalParamType :: String -> GenLLVM ()
writeLiteralValueEvalParamType code = do
    -- 0:[0 x i1]* bit array
    -- 1:i32 current index
    -- 2:i32 array length
    writeCode ("{[0 x i1]*,i32,i32}" ++ code)

writeNewConstantLiteralValue :: [Bool] -> GenLLVM Local
writeNewConstantLiteralValue bits
 | null bits = writeNewNilValue
 | otherwise = do
    bitArray <-
        writeNewLocal ("bitcast " ++ literalType bits ++ "* "
                                  ++ literalName bits ++ " to [0 x i1]*")
    value <- writeNewLiteralValue (Right (length bits)) bitArray
    return value

writeNewLiteralValueEvalParam :: [Bool] -> GenLLVM Local
writeNewLiteralValueEvalParam bits = do
    rawPtr <- writeAlloc (writeLiteralValueEvalParamType "")
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeLiteralValueEvalParamType "*"
    bitArrayPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 0"
    bitArray <-
        writeNewLocal ("bitcast " ++ literalType bits ++ "* "
                                  ++ literalName bits ++ " to [0 x i1]*")
    writeStore (writeCode "[0 x i1]*") (Left bitArray) bitArrayPtr
    currentIndexPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                          (Left evalParam) "i32 0,i32 1"
    writeStore (writeCode "i32") (Right "0") currentIndexPtr
    arraySizePtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                       (Left evalParam) "i32 0,i32 2"
    writeStore (writeCode "i32") (Right (show (length bits))) arraySizePtr
    return rawPtr

writeNewLiteralValue :: Either Local Int -> Local -> GenLLVM Local
writeNewLiteralValue arraySize bitArray = do
    (value,valueRawPtr) <- writeAllocateNewValue 3
    rawPtr <- writeAlloc (writeLiteralValueEvalParamType "")
    evalParamRawPtrPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left rawPtr) evalParamRawPtrPtr
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeLiteralValueEvalParamType "*"
    bitArrayPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 0"
    writeStore (writeCode "[0 x i1]*") (Left bitArray) bitArrayPtr
    currentIndexPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                          (Left evalParam) "i32 0,i32 1"
    writeStore (writeCode "i32") (Right "0") currentIndexPtr
    arraySizePtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                       (Left evalParam) "i32 0,i32 2"
    writeStore
        (writeCode "i32") (fmap show arraySize) arraySizePtr
    evalFuncPtr <- writeValueFieldPtr value 3
    writeStore
        (writeCode "{i2,i8*}(i8*,i8*)*") (Right "@evalLiteral") evalFuncPtr
    freeEvalParamFuncPtr <- writeValueFieldPtr value 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamLiteral") freeEvalParamFuncPtr
    return value

writeEvalLiteralValueDefn :: GenLLVM ()
writeEvalLiteralValueDefn = do
    writeCode "define private fastcc {i2,i8*} "
    writeCode "@evalLiteral(i8* %evalParam,i8* %value) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeLiteralValueEvalParamType "*"
    value <- writeNewLocal "bitcast i8* %value to "
    writeValueType "*"
    statusPtr <- writeValueFieldPtr value 1
    currentIndexPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                          (Left evalParam) "i32 0,i32 1"
    currentIndex <- writeLoad (writeCode "i32") currentIndexPtr
    arraySizePtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                       (Left evalParam) "i32 0,i32 2"
    arraySize <- writeLoad (writeCode "i32") arraySizePtr
    cmp <- writeNewLocal "icmp ult i32 "
    writeLocal currentIndex ","
    writeLocal arraySize ""
    (anotherBitLabelRef,nilLabelRef) <- writeBranch cmp

    writeNewLabelBack [anotherBitLabelRef]
    nextIndex <- writeNewLocal "add i32 1,"
    writeLocal currentIndex ""
    writeStore (writeCode "i32") (Left nextIndex) currentIndexPtr
    bitArrayPtr <- writeGetElementPtr (writeLiteralValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 0"
    bitArray <- writeLoad (writeCode "[0 x i1]*") bitArrayPtr
    bitPtr <- writeGetElementPtr (writeCode "[0 x i1]")
                                 (Left bitArray) "i32 0,i32 "
    writeLocal currentIndex ""
    bit <- writeLoad (writeCode "i1") bitPtr
    newStatus <- writeNewLocal "zext i1 "
    writeLocal bit " to i2"
    writeStore (writeCode "i2") (Left newStatus) statusPtr
    (nextValue,nextValueRawPtr) <- writeAllocateNewValue 3
    valueNextPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left nextValueRawPtr) valueNextPtr
    nextValueEvalParamPtr <- writeValueFieldPtr nextValue 2
    writeStore (writeCode "i8*") (Right "%evalParam") nextValueEvalParamPtr
    nextValueEvalFuncPtr <- writeValueFieldPtr nextValue 3
    writeStore (writeCode "{i2,i8*}(i8*,i8*)*")
               (Right "@evalLiteral") nextValueEvalFuncPtr
    freeEvalParamFuncPtr <- writeValueFieldPtr nextValue 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamLiteral") freeEvalParamFuncPtr
    retVal1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal newStatus ",0"
    retVal <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retVal1 ",i8* "
    writeLocal nextValueRawPtr ",1"
    writeCode " ret {i2,i8*} "
    writeLocal retVal ""

    writeNewLabelBack [nilLabelRef]
    writeFree (Right "%evalParam")
    writeStore (writeCode "i2") (Right "2") statusPtr
    retVal <- writeNewLocal "insertvalue {i2,i8*} undef,i2 2,0"
    writeCode " ret {i2,i8*} "
    writeLocal retVal ""
    writeCode " }"

writeFreeEvalParamLiteralValueDefn :: GenLLVM ()
writeFreeEvalParamLiteralValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamLiteral(i8* %evalParam) {"
    writeFree (Right "%evalParam")
    writeCode " ret void"
    writeCode " }"

writeConcatValueEvalParamType :: String -> GenLLVM ()
writeConcatValueEvalParamType code = do
    -- 0:value type* first
    -- 1:value type* rest
    writeCode "{"
    writeValueType "*,"
    writeValueType ("*}" ++ code)

writeNewConcatValueEvalParam :: Local -> Local -> GenLLVM Local
writeNewConcatValueEvalParam value1 value2 = do
    rawPtr <- writeAlloc (writeConcatValueEvalParamType "")
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeConcatValueEvalParamType "*"
    ptr1 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 0"
    writeStore (writeValueType "*") (Left value1) ptr1
    ptr2 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 1"
    writeStore (writeValueType "*") (Left value2) ptr2
    return rawPtr

writeNewConcatValue :: Local -> Local -> GenLLVM (Local,Local)
writeNewConcatValue value1 value2 = do
    (value,valueRawPtr) <- writeAllocateNewValue 3
    rawPtr <- writeAlloc (writeConcatValueEvalParamType "")
    evalParamRawPtrPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left rawPtr) evalParamRawPtrPtr
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal rawPtr " to "
    writeConcatValueEvalParamType "*"
    ptr1 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 0"
    writeStore (writeValueType "*") (Left value1) ptr1
    ptr2 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 1"
    writeStore (writeValueType "*") (Left value2) ptr2
    evalFuncPtr <- writeValueFieldPtr value 3
    writeStore (writeCode "{i2,i8*}(i8*,i8*)*")
               (Right "@evalConcat") evalFuncPtr
    freeEvalParamFuncPtr <- writeValueFieldPtr value 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamConcat") freeEvalParamFuncPtr
    return (value,valueRawPtr)

writeEvalConcatValueDefn :: GenLLVM ()
writeEvalConcatValueDefn = do
    writeCode "define private fastcc {i2,i8*} "
    writeCode "@evalConcat(i8* %evalParam,i8* %value) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeConcatValueEvalParamType "*"
    value <- writeNewLocal "bitcast i8* %value to "
    writeValueType "*"
    ptr1 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 0"
    value1 <- writeLoad (writeValueType "*") ptr1
    ptr2 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 1"
    value2 <- writeLoad (writeValueType "*") ptr2

    (status1,nextValue1) <- writeForceEval value1
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status1 ""
    (abortLabelRef1,okLabelRef1) <- writeBranch cmp

    abortLabel <- writeNewLabelBack [abortLabelRef1]
    writeCode " call void @abort() noreturn"
    writeCode " ret {i2,i8*} undef"

    writeNewLabelBack [okLabelRef1]
    cmp <- writeNewLocal "icmp ne i2 2,"
    writeLocal status1 ""
    (nonnilLabelRef1,nilLabelRef1) <- writeBranch cmp

    writeNewLabelBack [nonnilLabelRef1]
    writeAddRef nextValue1
    writeUnref (Left value1)
    writeStore (writeValueType "*") (Left nextValue1) ptr1
    (newNext,newNextRawPtr) <- writeAllocateNewValue 3
    nextEvalParamPtr <- writeValueFieldPtr newNext 2
    writeStore (writeCode "i8*") (Right "%evalParam") nextEvalParamPtr
    evalFuncPtr <- writeValueFieldPtr newNext 3
    writeStore (writeCode "{i2,i8*}(i8*,i8*)*")
               (Right "@evalConcat") evalFuncPtr
    freeEvalParamFuncPtr <- writeValueFieldPtr newNext 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamConcat") freeEvalParamFuncPtr
    valueStatusPtr <- writeValueFieldPtr value 1
    writeStore (writeCode "i2") (Left status1) valueStatusPtr
    valueNextPtrPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left newNextRawPtr) valueNextPtrPtr
    retValue1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal status1 ",0"
    retValue <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retValue1 ",i8* "
    writeLocal newNextRawPtr ",1"
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""

    writeNewLabelBack [nilLabelRef1]
    writeUnref (Left value1)
    writeFree (Right "%evalParam")
    statusPtr2 <- writeValueFieldPtr value2 1
    status2 <- writeLoad (writeCode "i2") statusPtr2
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status2 ""
    (unevaluatedLabelRef2,evaluatedLabelRef2) <- writeBranch cmp

    writeNewLabelBack [unevaluatedLabelRef2]
    value2RawPtr <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal value2 " to i8*"
    evalParamPtr2 <- writeValueFieldPtr value2 2
    evalParam2 <- writeLoad (writeCode "i8*") evalParamPtr2
    evalFuncPtr2 <- writeValueFieldPtr value2 3
    evalFunc2 <- writeLoad (writeCode "{i2,i8*}(i8*,i8*)*") evalFuncPtr2
    refCountPtr <- writeValueFieldPtr value2 0
    refCount <- writeLoad (writeCode "i32") refCountPtr
    cmp <- writeNewLocal "icmp ule i32 "
    writeLocal refCount ",1"
    (noOtherRefsLabelRef2,hasOtherRefsLabelRef2) <- writeBranch cmp

    writeNewLabelBack [noOtherRefsLabelRef2]
    writeFree (Left value2RawPtr)
    when debugIndirectCalls (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                       ++ ",i32 0,i32 0),i8 101,{i2,i8*}(i8*,i8*)* ")
        writeLocal evalFunc2 ")"
        writeNewLocal "call i32 @fflush(i8* null)"
        return ())
    retValue <- writeNewLocal "musttail call fastcc {i2,i8*} "
    writeLocal evalFunc2 "(i8* "
    writeLocal evalParam2 ",i8* %value)"
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""

    hasOtherRefsLabel2 <- writeNewLabelBack [hasOtherRefsLabelRef2]
    when debugIndirectCalls (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([7 x i8],[7 x i8]* @debugIndirectCallsFmt"
                       ++ ",i32 0,i32 0),i8 69,{i2,i8*}(i8*,i8*)* ")
        writeLocal evalFunc2 ")"
        writeNewLocal "call i32 @fflush(i8* null)"
        return ())
    value2Eval <- writeNewLocal "call fastcc {i2,i8*} "
    writeLocal evalFunc2 "(i8* "
    writeLocal evalParam2 ",i8* %value)"
    value2Status <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal value2Eval ",0"
    value2NextRawPtr <- writeNewLocal "extractvalue {i2,i8*} "
    writeLocal value2Eval ",1"
    writeCode " br label "
    copyLabelRef2 <- writeForwardRefLabel

    evaluatedLabel2 <- writeNewLabelBack [evaluatedLabelRef2]
    value2NextPtr <- writeValueFieldPtr value2 2
    value2NextRawPtr2 <- writeLoad (writeCode "i8*") value2NextPtr
    writeCode " br label "
    copyLabelRef3 <- writeForwardRefLabel

    writeNewLabelBack [copyLabelRef2,copyLabelRef3]
    copyStatus2 <- writeNewLocal "phi i2 ["
    writeLocal value2Status ","
    writeLabelRef hasOtherRefsLabel2 "],["
    writeLocal status2 ","
    writeLabelRef evaluatedLabel2 "]"
    copyNextRawPtr2 <- writeNewLocal "phi i8* ["
    writeLocal value2NextRawPtr ","
    writeLabelRef hasOtherRefsLabel2 "],["
    writeLocal value2NextRawPtr2 ","
    writeLabelRef evaluatedLabel2 "]"
    valueStatusPtr <- writeValueFieldPtr value 1
    writeStore (writeCode "i2") (Left copyStatus2) valueStatusPtr
    cmp <- writeNewLocal "icmp eq i2 2,"
    writeLocal copyStatus2 ""
    (copy2NilLabelRef,copy2NotNilLabelRef) <- writeBranch cmp

    writeNewLabelBack [copy2NilLabelRef]
    writeUnref (Left value2)
    retValue <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal copyStatus2 ",0"
    writeCode " ret {i2,i8*} "
    writeLocal retValue ""

    writeNewLabelBack [copy2NotNilLabelRef]
    valueNextPtrPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left copyNextRawPtr2) valueNextPtrPtr
    copyNext <- writeNewLocal "bitcast i8* "
    writeLocal copyNextRawPtr2 " to "
    writeValueType "*"
    writeAddRef copyNext
    writeUnref (Left value2)
    retValue1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal copyStatus2 ",0"
    retValue <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retValue1 ",i8* "
    writeLocal copyNextRawPtr2 ",1"
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
    ptr1 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 0"
    value1 <- writeLoad (writeValueType "*") ptr1
    writeUnref (Left value1)
    ptr2 <- writeGetElementPtr (writeConcatValueEvalParamType "")
                               (Left evalParam) "i32 0,i32 1"
    value2 <- writeLoad (writeValueType "*") ptr2
    writeUnref (Left value2)
    writeFree (Right "%evalParam")
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
    (value,_) <- writeAllocateNewValue 3
    evalParamRawPtr <- writeAlloc (writeFileValueEvalParamType "")
    evalParam <- writeNewLocal "bitcast i8* "
    writeLocal evalParamRawPtr " to "
    writeFileValueEvalParamType "*"
    fdPtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                (Left evalParam) "i32 0,i32 0"
    writeStore (writeCode "i32") fd fdPtr
    bitIndexPtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 1"
    writeStore (writeCode "i8") (Right "-1") bitIndexPtr
    evalParamPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left evalParamRawPtr) evalParamPtr
    evalFuncPtr <- writeValueFieldPtr value 3
    writeCode " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** "
    writeLocal evalFuncPtr ""
    freeEvalParamFuncPtr <- writeValueFieldPtr value 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamFile") freeEvalParamFuncPtr
    return value

writeEvalFileValueDefn :: GenLLVM ()
writeEvalFileValueDefn = do
    writeCode "define private fastcc {i2,i8*} "
    writeCode "@evalFile(i8* %evalParam,i8* %value) {"
    entryLabel <- writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeFileValueEvalParamType "*"
    value <- writeNewLocal "bitcast i8* %value to "
    writeValueType "*"
    statusPtr <- writeValueFieldPtr value 1
    bitIndexPtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                      (Left evalParam) "i32 0,i32 1"
    bytePtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                  (Left evalParam) "i32 0,i32 2"
    bitIndex <- writeLoad (writeCode "i8") bitIndexPtr
    cmp <- writeNewLocal "icmp slt i8 "
    writeLocal bitIndex ",0"
    (readNewByteLabelRef,evalNextBitLabelRef) <- writeBranch cmp

    readNewByteLabel <- writeNewLabelBack [readNewByteLabelRef]
    fdPtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                (Left evalParam) "i32 0,i32 0"
    fd <- writeLoad (writeCode "i32") fdPtr
    readResult <- writeNewLocal "call i32 @read(i32 "
    writeLocal fd ",i8* "
    writeLocal bytePtr ",i32 1)"
    cmp <- writeNewLocal "icmp eq i32 1,"
    writeLocal readResult ""
    (evalNextBitLabelRef2,eofLabelRef) <- writeBranch cmp

    writeNewLabelBack [evalNextBitLabelRef,evalNextBitLabelRef2]
    currentBitIndex <- writeNewLocal "phi i8 ["
    writeLocal bitIndex ","
    writeLabelRef entryLabel "],[7,"
    writeLabelRef readNewByteLabel "]"
    nextBitIndex <- writeNewLocal "sub i8 "
    writeLocal currentBitIndex ",1"
    writeStore (writeCode "i8") (Left nextBitIndex) bitIndexPtr
    byte <- writeLoad (writeCode "i8") bytePtr
    shiftedByte <- writeNewLocal "lshr i8 "
    writeLocal byte ","
    writeLocal currentBitIndex ""
    almostNewStatus <- writeNewLocal "trunc i8 "
    writeLocal shiftedByte " to i2"
    newStatus <- writeNewLocal "and i2 1,"
    writeLocal almostNewStatus ""
    writeStore (writeCode "i2") (Left newStatus) statusPtr
    (nextValue,nextValueRawPtr) <- writeAllocateNewValue 3
    nextValuePtrPtr <- writeValueFieldPtr value 2
    writeStore (writeCode "i8*") (Left nextValueRawPtr) nextValuePtrPtr
    nextValueEvalParamPtr <- writeValueFieldPtr nextValue 2
    writeStore (writeCode "i8*") (Right "%evalParam") nextValueEvalParamPtr
    evalFuncPtr <- writeValueFieldPtr nextValue 3
    writeStore (writeCode "{i2,i8*}(i8*,i8*)*") (Right "@evalFile") evalFuncPtr
    freeEvalParamFuncPtr <- writeValueFieldPtr nextValue 4
    writeStore (writeCode "void(i8*)*")
               (Right "@freeEvalParamFile") freeEvalParamFuncPtr
    retVal1 <- writeNewLocal "insertvalue {i2,i8*} undef,i2 "
    writeLocal newStatus ",0"
    retVal <- writeNewLocal "insertvalue {i2,i8*} "
    writeLocal retVal1 ",i8* "
    writeLocal nextValueRawPtr ",1"
    writeCode " ret {i2,i8*} "
    writeLocal retVal ""

    writeNewLabelBack [eofLabelRef]
    writeNewLocal "call i32 @close(i32 "
    writeLocal fd ")"
    writeFree (Right "%evalParam")
    writeStore (writeCode "i2") (Right "2") statusPtr
    retVal <- writeNewLocal "insertvalue {i2,i8*} undef,i2 2,0"
    writeCode " ret {i2,i8*} "
    writeLocal retVal ""
    writeCode " }"

writeFreeEvalParamFileValueDefn :: GenLLVM ()
writeFreeEvalParamFileValueDefn = do
    writeCode "define private fastcc void "
    writeCode "@freeEvalParamFile(i8* %evalParam) {"
    writeNewLabel
    evalParam <- writeNewLocal "bitcast i8* %evalParam to "
    writeFileValueEvalParamType "*"
    fdPtr <- writeGetElementPtr (writeFileValueEvalParamType "")
                                (Left evalParam) "i32 0,i32 0"
    fd <- writeLoad (writeCode "i32") fdPtr
    writeNewLocal "call i32 @close(i32 "
    writeLocal fd ")"
    writeFree (Right "%evalParam")
    writeCode " ret void"
    writeCode " }"

writeNewNilValue :: GenLLVM Local
writeNewNilValue = do
    (value,_) <- writeAllocateNewValue 2
    return value

genMain :: String -> [Def] -> String
genMain name (Def params _:_) = genLLVM (do
    writeCode "define void @main(i32 %argc,i8** %argv) {"
    writeNewLabel
    writeInitDebugMemory
    args <- if null params
        then return []
        else do
            nilValue <- writeNewNilValue
            stdinValue <- writeNewFileValue (Right "0")
            args <- mapM (writeArg nilValue stdinValue) [1 .. length params]
            writeUnref (Left nilValue)
            writeUnref (Left stdinValue)
            return args
    result <- writeNewLocal "call fastcc "
    writeValueType "* "
    writeName name
    writeCode "("
    zipWithM_ (\ comma arg -> do
            writeCode comma
            writeValueType "* "
            writeLocal arg "")
        ("":repeat ",") args
    writeCode ")"
    writeCode " br label "
    entryLabelRef <- writeForwardRefLabel

    entryLabel <- writeNewLabelBack [entryLabelRef]
    buffer <- writeNewLocal "alloca i8,i32 1"
    writeCode " br label "
    loopLabelRef <- writeForwardRefLabel

    loopLabel <- writeNewLabelBack [loopLabelRef]
    (value,valuePhiRef) <-
        writePhi (writeValueType "*") (Left result) entryLabel
    (bitIndex,bitIndexPhiRef) <-
        writePhi (writeCode "i8") (Right "7") entryLabel
    writeCode ",[7,"
    bitIndexPhiLabelRef <- writeForwardRefLabel
    writeCode "]"
    (byte,bytePhiRef) <- writePhi (writeCode "i8") (Right "0") entryLabel
    writeCode ",[0,"
    bytePhiLabelRef <- writeForwardRefLabel
    writeCode "]"

    (status,nextValue) <- writeForceEval value
    cmp <- writeNewLocal "icmp eq i2 3,"
    writeLocal status ""
    (abortLabelRef,okLabelRef) <- writeBranch cmp

    writeNewLabelBack [abortLabelRef]
    writeCode " call void @abort() noreturn"
    writeCode " ret void"

    writeNewLabelBack [okLabelRef]
    cmp <- writeNewLocal "icmp eq i2 2,"
    writeLocal status ""
    (eofLabelRef,nextBitLabelRef) <- writeBranch cmp

    writeNewLabelBack [eofLabelRef]
    writeUnref (Left value)
    writeFinalizeDebugMemory
    writeCode " call void @exit(i32 0) noreturn"
    writeCode " ret void"

    nextBitLabel <- writeNewLabelBack [nextBitLabelRef]
    writeAddRef nextValue
    writeUnref (Left value)
    nextBit <- writeNewLocal "zext i2 "
    writeLocal status " to i8"
    shiftedBit <- writeNewLocal "shl i8 "
    writeLocal nextBit ","
    writeLocal bitIndex ""
    nextByte <- writeNewLocal "or i8 "
    writeLocal byte ","
    writeLocal shiftedBit ""
    nextBitIndex <- writeNewLocal "sub i8 "
    writeLocal bitIndex ",1"
    valuePhiRef nextValue nextBitLabel
    bitIndexPhiRef nextBitIndex nextBitLabel
    bytePhiRef nextByte nextBitLabel
    cmp <- writeNewLocal "icmp sge i8 "
    writeLocal nextBitIndex ",0"
    (continueRef,nextByteRef) <- writeBranch cmp
    continueRef loopLabel

    nextByteLabel <-
        writeNewLabelBack [nextByteRef,bitIndexPhiLabelRef,bytePhiLabelRef]
    valuePhiRef nextValue nextByteLabel
    writeStore (writeCode "i8") (Left nextByte) buffer
    writeNewLocal "call i32 @write(i32 1,i8* "
    writeLocal buffer ",i32 1)"
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
        filename <- writeLoad (writeCode "i8*") filenamePtr
        fd <- writeNewLocal "call i32 @open(i8* "
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
        writeCode " ret void"

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

writeAlloc :: GenLLVM () -> GenLLVM Local
writeAlloc writeType = do
    sizePtr <- writeGetElementPtr writeType (Right "null") "i32 1"
    size <- writeNewLocal "ptrtoint "
    writeType
    writeCode "* "
    writeLocal sizePtr " to i32"
    writeMalloc size

writeMalloc :: Local -> GenLLVM Local
writeMalloc size = do
    result <- writeNewLocal "call "
    if debugMemory
        then writeCode "fastcc i8* @debugMalloc"
        else writeCode "i8* @malloc"
    writeCode "(i32 "
    writeLocal size ")"
    return result

writeFree :: Either Local String -> GenLLVM ()
writeFree value = do
    if debugMemory
        then writeCode " call fastcc void @debugFree"
        else writeCode " call void @free"
    writeCode "(i8* "
    either (flip writeLocal "") writeCode value
    writeCode ")"

writeInitDebugMemory :: GenLLVM ()
writeInitDebugMemory | not debugMemory = return () | otherwise = do
    writeCode " store i32 0,i32* @debugMemoryCount"

writeFinalizeDebugMemory :: GenLLVM ()
writeFinalizeDebugMemory | not debugMemory = return () | otherwise = do
    count <- writeNewLocal "load i32,i32* @debugMemoryCount"
    fmt <- writeNewLocal
        "getelementptr [10 x i8],[10 x i8]* @debugMemoryOutputFmt,i32 0,i32 0"
    writeNewLocal "call i32(i8*,...) @printf(i8* "
    writeLocal fmt ",i32 "
    writeLocal count ")"

writeDebugDefns :: GenLLVM ()
writeDebugDefns
  | not (debugMemory || debugIndirectCalls || debugDeref) = return ()
  | otherwise = do
    writeCode "@debugMemoryCount = private global i32 0 "
    writeCode "@debugMemoryOutputFmt = private constant [10 x i8] "
    writeCode "c\"count=%d\\0a\\00\" "
    writeCode "declare i32 @printf(i8*,...)"
    writeCode "declare i32 @fflush(i8*)"
    when debugMemoryAllocs (do
        writeCode ("@debugMemoryAllocsFmt = private constant "
                   ++ "[9 x i8] c\"%c%d:%x\\0A\\00\""))
    when debugIndirectCalls (do
        writeCode ("@debugIndirectCallsFmt = private constant "
                   ++ "[7 x i8] c\"%c:%x\\0A\\00\""))
    when debugDeref (do
        writeCode ("@debugDerefFmt = private constant "
                   ++ "[12 x i8] c\"*%c%c%c:%x\\0A\\00\""))

writeDebugMemoryMallocDefn :: GenLLVM ()
writeDebugMemoryMallocDefn | not debugMemory = return () | otherwise = do
    writeCode "define fastcc i8* @debugMalloc(i32 %size) {"
    writeNewLabel
    oldCount <- writeNewLocal "load i32,i32* @debugMemoryCount"
    newCount <- writeNewLocal "add i32 1,"
    writeLocal oldCount ""
    writeCode " store i32 "
    writeLocal newCount ",i32* @debugMemoryCount"
    result <- writeNewLocal "call i8* @malloc(i32 %size)"
    when debugMemoryAllocs (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([9 x i8],[9 x i8]* @debugMemoryAllocsFmt"
                       ++ ",i32 0,i32 0),i8 65,i32 ")
        writeLocal newCount ",i8* "
        writeLocal result ")"
        writeNewLocal "call i32(i8*) @fflush(i8* null)"
        return ())
    writeCode " ret i8* "
    writeLocal result ""
    writeCode " }"

writeDebugMemoryFreeDefn :: GenLLVM ()
writeDebugMemoryFreeDefn | not debugMemory = return () | otherwise = do
    writeCode "define fastcc void @debugFree(i8* %value) {"
    writeNewLabel
    oldCount <- writeNewLocal "load i32,i32* @debugMemoryCount"
    newCount <- writeNewLocal "sub i32 "
    writeLocal oldCount ",1"
    writeCode " store i32 "
    writeLocal newCount ",i32* @debugMemoryCount"
    when debugMemoryAllocs (do
        writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                       ++ "([9 x i8],[9 x i8]* @debugMemoryAllocsFmt"
                       ++ ",i32 0,i32 0),i8 70,i32 ")
        writeLocal newCount ",i8* %value)"
        writeNewLocal "call i32(i8*) @fflush(i8* null)"
        return ())
    writeCode " call void @free(i8* %value)"
    writeCode " ret void"
    writeCode " }"

writeDebugDeref :: String -> String -> GenLLVM ()
writeDebugDeref tag value | not debugDeref = return () | otherwise = do
    rawPtr <- writeNewLocal ("select i1 1,i8* " ++ value ++ ",i8* " ++ value)
    writeDebugDerefRawPtr tag rawPtr

writeDebugDerefValue :: String -> Local -> GenLLVM ()
writeDebugDerefValue tag value | not debugDeref = return () | otherwise = do
    rawPtr <- writeNewLocal "bitcast "
    writeValueType "* "
    writeLocal value " to i8*"
    writeDebugDerefRawPtr tag rawPtr

writeDebugDerefRawPtr :: String -> Local -> GenLLVM ()
writeDebugDerefRawPtr tag rawPtr | not debugDeref = return () | otherwise = do
    let (tag1:tag2:tag3:_) = tag ++ repeat ' '
    writeNewLocal ("call i32(i8*,...) @printf(i8* getelementptr "
                   ++ "([12 x i8],[12 x i8]* @debugDerefFmt"
                   ++ ",i32 0,i32 0),i8 " ++ show (ord tag1) ++ ",i8 "
                   ++ show (ord tag2) ++ ",i8 " ++ show (ord tag3) ++ ",i8* ")
    writeLocal rawPtr ")"
    writeNewLocal "call i32(i8*) @fflush(i8* null)"
    return ()
