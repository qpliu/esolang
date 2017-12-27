{-# LANGUAGE RecursiveDo #-}

module CodeGen.CodeGen(
    codeGen
)
where

import Control.Monad(foldM)
import Control.Monad.Fix(MonadFix)
import Data.String(fromString)

import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand)
import LLVM.AST.Type(i8,i32,ptr,void)
import LLVM.IRBuilder.Instruction(alloca,bitcast,br,condBr,gep,icmp,load,phi,ptrtoint,retVoid,store)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),extern,function)
import LLVM.IRBuilder.Monad(MonadIRBuilder,block)

import AST.AST(
    Module(Library,Program),
    Routine(Routine),
    Var(Var),
    Val(Val,NewVal),
    Statement(LetEq,LetAddEdge,LetRemoveEdge,If,Call,Return,DoLoop,DoEdges,Exit),
    IfBranch(IfEq,IfEdge,IfElse))
import CodeGen.DGOLLibs(
    dgolLibDefs)
import CodeGen.Runtime(
    runtimeDecls,
    runtimeDefs,
    memset,malloc,free,
    newNode,hasEdge,addEdge,removeEdge)
import CodeGen.Types(
    pNodeType,ppNodeType,pppNodeType,
    frameType,pFrameType,
    doEdgesIteratorType,pDoEdgesIteratorType)
import CodeGen.Util(
    eq,uge,
    intConst,nullConst,
    functionRef,
    call)


codeGen :: Module -> [String] -> ModuleBuilder ()
codeGen (Library name subroutines externs) _ = do
    runtimeDecls
    mapM_ (declareExtern []) externs
    mapM_ (defineSubroutine name) subroutines
    mapM_ (defineExportedSubroutine name) subroutines
    return ()
codeGen (Program name subroutines program externs) libs = do
    runtimeDefs
    libs <- foldM defineLibs [] libs
    mapM_ (declareExtern libs) externs
    mapM_ (defineSubroutine name) subroutines
    defineMain program
    return ()

declareExtern :: [String] -> (String,String) -> ModuleBuilder Operand
declareExtern libs (mod,rout)
  | elem mod libs =
        return $ nullConst void
  | otherwise =
        extern (fromString (mod ++ "." ++ rout)) [pFrameType] void

defineLibs :: [String] -> String -> ModuleBuilder [String]
defineLibs libs lib =
    maybe (return libs) (>> return (lib:libs)) (dgolLibDefs lib)

defineSubroutine :: String -> Routine -> ModuleBuilder Operand
defineSubroutine moduleName routine@(Routine name args stmts exported varCount doEdgesCount callArgsMaxCount) = function (routineName False moduleName name) [(pFrameType,NoParameterName)] void $ \ [callerFrame] -> mdo
    (frame,varArray,doEdgesArray,callArgsArray,exitLabel) <- functionPrelude routine frame entryLabel
    entryLabel <- block
    callerCallArgsArrayPtr <- gep callerFrame [intConst 32 0,intConst 32 6]
    callerCallArgsArray <- load callerCallArgsArrayPtr 0
    callerCallArgsCountPtr <- gep callerFrame [intConst 32 0,intConst 32 5]
    callerCallArgsCount <- load callerCallArgsCountPtr 0
    codeGenStmts frame varArray doEdgesArray callArgsArray callerCallArgsArray callerCallArgsCount exitLabel stmts

defineExportedSubroutine :: String -> Routine -> ModuleBuilder Operand
defineExportedSubroutine moduleName (Routine name args stmts exported varCount doEdgesCount callArgsMaxCount)
  | not exported =
        return $ nullConst void
  | otherwise = function (routineName True moduleName name) [(pFrameType,NoParameterName)] void $ \ [frame] -> do
        call (functionRef (routineName False moduleName name) [pFrameType] void) [frame]
        retVoid

defineMain :: Routine -> ModuleBuilder Operand
defineMain routine@(Routine name args stmts exported varCount doEdgesCount callArgsMaxCount) = function (fromString "main") [] void $ \ _ -> mdo
    (frame,varArray,doEdgesArray,callArgsArray,exitLabel) <- functionPrelude routine (nullConst pFrameType) entryLabel
    entryLabel <- block
    let callerCallArgsArray = nullConst pppNodeType
    let callerCallArgsCount = intConst 32 0
    codeGenStmts frame varArray doEdgesArray callArgsArray callerCallArgsArray callerCallArgsCount exitLabel stmts

routineName :: Bool -> String -> String -> Name
routineName exported mod rout =
  fromString $ (if exported then "" else ".") ++ mod ++ "." ++ rout

functionPrelude :: (MonadIRBuilder m, MonadFix m) => Routine -> Operand -> Name -> m (Operand,Operand,Operand,Operand,Name)
functionPrelude (Routine name args stmts exported varCount doEdgesCount callArgsMaxCount) callerFrame entryLabel = do
    frame <- alloca frameType Nothing 0

    callerFramePtr <- gep frame [intConst 32 0,intConst 32 0]
    store callerFramePtr 0 callerFrame

    varCountPtr <- gep frame [intConst 32 0,intConst 32 1]
    store varCountPtr 0 (intConst 32 varCount)
    varArray <- if varCount == 0
      then do
        return $ nullConst ppNodeType
      else do
        varArray <- alloca pNodeType (Just (intConst 32 varCount)) 0
        varArraySizeofPtr <- gep (nullConst ppNodeType) [intConst 32 varCount]
        varArraySizeof <- ptrtoint varArraySizeofPtr i32
        varArrayRawPtr <- bitcast varArray (ptr i8)
        call memset [varArrayRawPtr,intConst 8 0,varArraySizeof,intConst 32 0,intConst 1 0]
        return varArray
    varArrayPtr <- gep frame [intConst 32 0,intConst 32 2]
    store varArrayPtr 0 varArray

    doEdgesCountPtr <- gep frame [intConst 32 0,intConst 32 3]
    store doEdgesCountPtr 0 (intConst 32 doEdgesCount)
    doEdgesArray <- if doEdgesCount == 0
      then do
        return $ nullConst pDoEdgesIteratorType
      else do
        doEdgesArray <- alloca doEdgesIteratorType (Just (intConst 32 doEdgesCount)) 0
        doEdgesArraySizeofPtr <- gep (nullConst pDoEdgesIteratorType) [intConst 32 doEdgesCount]
        doEdgesArraySizeof <- ptrtoint doEdgesArraySizeofPtr i32
        doEdgesArrayRawPtr <- bitcast doEdgesArray (ptr i8)
        call memset [doEdgesArrayRawPtr,intConst 8 0,doEdgesArraySizeof,intConst 32 0,intConst 1 0]
        return doEdgesArray
    doEdgesArrayPtr <- gep frame [intConst 32 0,intConst 32 4]
    store doEdgesArrayPtr 0 doEdgesArray

    callArgsMaxCountPtr <- gep frame [intConst 32 0,intConst 32 5]
    store callArgsMaxCountPtr 0 (intConst 32 callArgsMaxCount)
    callArgsArray <- if callArgsMaxCount == 0
      then do
        return $ nullConst pppNodeType
      else do
        callArgsArray <- alloca ppNodeType (Just (intConst 32 callArgsMaxCount)) 0
        callArgsArraySizeofPtr <- gep (nullConst pppNodeType) [intConst 32 callArgsMaxCount]
        callArgsArraySizeof <- ptrtoint callArgsArraySizeofPtr i32
        callArgsArrayRawPtr <- bitcast callArgsArray (ptr i8)
        call memset [callArgsArrayRawPtr,intConst 8 0,callArgsArraySizeof,intConst 32 0,intConst 1 0]
        return callArgsArray
    callArgsArrayPtr <- gep frame [intConst 32 0,intConst 32 6]
    store callArgsArrayPtr 0 callArgsArray

    br entryLabel

    exitLabel <- block
    mapM_ (\ doEdgesIndex -> mdo
        edgesArrayPtr <- gep doEdgesArray [intConst 32 doEdgesIndex,intConst 32 2]
        edgesArray <- load edgesArrayPtr 0
        edgesArrayCheck <- icmp eq edgesArray (nullConst ppNodeType)
        condBr edgesArrayCheck doneLabel freeLabel

        freeLabel <- block
        edgesArrayRawPtr <- bitcast edgesArray (ptr i8)
        call free [edgesArrayRawPtr]
        br doneLabel

        doneLabel <- block
        return ()
        ) [0 .. doEdgesCount - 1]
    retVoid

    return (frame,varArray,doEdgesArray,callArgsArray,exitLabel)

codeGenStmts :: MonadIRBuilder m => Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> Name -> [Statement] -> m ()
codeGenStmts frame varArray doEdgesArray callArgsArray callerCallArgsArray callerCallArgsCount exitLabel stmts = do
    -- ...
    br exitLabel
