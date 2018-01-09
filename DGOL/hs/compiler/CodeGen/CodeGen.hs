{-# LANGUAGE RecursiveDo #-}

module CodeGen.CodeGen(
    codeGen
)
where

import Control.Monad(foldM,foldM_,when,zipWithM_)
import Control.Monad.Fix(MonadFix)
import Data.String(fromString)

import LLVM.AST(Definition(MetadataNodeDefinition))
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Metadata(MDString,MDNode,MDValue),MetadataNode(MetadataNode,MetadataNodeReference),MetadataNodeID(MetadataNodeID),Operand)
import LLVM.AST.Type(i8,i32,ptr,void)
import LLVM.IRBuilder.Instruction(add,alloca,bitcast,br,condBr,gep,icmp,load,phi,ptrtoint,retVoid,store)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),emitDefn,extern,function)
import LLVM.IRBuilder.Monad(MonadIRBuilder,block)

import AST.AST(
    Module(Library,Program),
    Routine(Routine),
    Var(Var),
    Val(Val,NewVal),
    Statement(LetEq,LetAddEdge,LetRemoveEdge,If,Call,Return,DoLoop,DoEdges,Exit),
    IfBranch(IfEq,IfEdge,IfElse),ifBranchStmts)
import CodeGen.DGOLLibs(
    dgolLibDefs)
import CodeGen.Runtime(
    runtimeDecls,
    runtimeDefs,
    memset,memcpy,malloc,free,debugAddr,
    newNode,hasEdge,addEdge,removeEdge,compact,
    trace,traceLabel)
import CodeGen.Types(
    pNodeType,ppNodeType,pppNodeType,
    frameType,pFrameType,
    doEdgesIteratorType,pDoEdgesIteratorType)
import CodeGen.Util(
    eq,uge,
    intConst,nullConst,
    functionRef,functionAlias,
    call,
    debugMetadata,subprogramMetadata,lineNumberMetadata,localVariableMetadata)


codeGen :: Module -> [String] -> ModuleBuilder ()
codeGen (Library name subroutines externs srcFileName srcDir) _ = do
    (fileMetadataID,compileUnitMetadataID,nextNodeID) <- debugMetadata srcFileName srcDir
    (nodePtrTypeMetadataID,nextNodeID) <- debugTypeMetadata nextNodeID
    runtimeDecls
    mapM_ (declareExtern []) externs
    foldM_ (defineSubroutine fileMetadataID compileUnitMetadataID nodePtrTypeMetadataID name) nextNodeID subroutines
    mapM_ (defineExportedSubroutine name) subroutines
    return ()
codeGen (Program name subroutines program externs srcFileName srcDir) libs = do
    (fileMetadataID,compileUnitMetadataID,nextNodeID) <- debugMetadata srcFileName srcDir
    (nodePtrTypeMetadataID,nextNodeID) <- debugTypeMetadata nextNodeID
    runtimeDefs
    libs <- foldM defineLibs [] libs
    mapM_ (declareExtern libs) externs
    nextNodeID <- foldM (defineSubroutine fileMetadataID compileUnitMetadataID nodePtrTypeMetadataID name) nextNodeID subroutines
    defineMain fileMetadataID compileUnitMetadataID nodePtrTypeMetadataID nextNodeID program
    return ()

declareExtern :: [String] -> (String,String) -> ModuleBuilder ()
declareExtern libs (mod,rout)
  | elem mod libs = return ()
  | otherwise = do
        extern (fromString (mod ++ "." ++ rout)) [pFrameType] void
        return ()

defineLibs :: [String] -> String -> ModuleBuilder [String]
defineLibs libs lib =
    maybe (return libs) (>> return (lib:libs)) (dgolLibDefs lib)

defineSubroutine :: MetadataNodeID -> MetadataNodeID -> MetadataNodeID -> String -> MetadataNodeID -> Routine -> ModuleBuilder MetadataNodeID
defineSubroutine fileMetadataID compileUnitMetadataID nodePtrTypeMetadataID moduleName metadataNodeID routine@(Routine name args stmts exported vars doEdgesCount callArgsMaxCount lineNumber endLineNumber) = do
    let varCount = length vars
    function (routineName False moduleName name) [(pFrameType,NoParameterName)] void $ \ [callerFrame] -> mdo
        (frame,varArray,doEdgesArray,callArgsArray,callArgsArraySizeof,exitLabel) <- functionPrologue metadataNodeID nodePtrTypeMetadataID routine callerFrame entryLabel (varCount > 1)
        entryLabel <- block
        (callerCallArgsCount,callerCallArgsArray) <- if null args
          then do
            return (intConst 32 0,nullConst pppNodeType)
          else do
            callerCallArgsArrayPtr <- gep callerFrame [intConst 32 0,intConst 32 6]
            callerCallArgsArray <- load callerCallArgsArrayPtr 0
            callerCallArgsCountPtr <- gep callerFrame [intConst 32 0,intConst 32 5]
            callerCallArgsCount <- load callerCallArgsCountPtr 0
            return (callerCallArgsCount,callerCallArgsArray)
        mapM_ (\ var@(Var name index isCallArg) -> do
            (varAddr,arg) <- if not isCallArg
              then do
                varAddr <- gep varArray [intConst 32 index]
                return (varAddr,0)
              else mdo
                indexCheck <- icmp uge (intConst 32 index) callerCallArgsCount
                condBr indexCheck useLocalVarLabel checkCallerCallArgsArrayLabel

                checkCallerCallArgsArrayLabel <- block
                callerArgPtrPtr <- gep callerCallArgsArray [intConst 32 index]
                -- workaround for
                -- *** Exception: gep: Can't index into a NamedTypeReference (Name "frame")
                callerArgPtr_workaround <- load callerArgPtrPtr 0
                callerArgPtr <- bitcast callerArgPtr_workaround ppNodeType
                callerArgPtrCheck <- icmp eq callerArgPtr (nullConst ppNodeType)
                condBr callerArgPtrCheck useLocalVarLabel getVarAddrLabel

                useLocalVarLabel <- block
                localVarAddr <- gep varArray [intConst 32 index]
                br getVarAddrLabel

                getVarAddrLabel <- block
                varAddr <- phi [(callerArgPtr,checkCallerCallArgsArrayLabel),(localVarAddr,useLocalVarLabel)]
                return (varAddr,index+1)
            call debugAddr (localVariableMetadata varAddr name arg nodePtrTypeMetadataID metadataNodeID)
            ) [] -- TODO ... vars
        codeGenStmts metadataNodeID moduleName frame varArray doEdgesArray callArgsArray callArgsArraySizeof callerCallArgsArray callerCallArgsCount exitLabel stmts
    nextMetadataNodeID <- subprogramMetadata fileMetadataID compileUnitMetadataID metadataNodeID (moduleName ++ "." ++ name) lineNumber
    return nextMetadataNodeID

defineExportedSubroutine :: String -> Routine -> ModuleBuilder ()
defineExportedSubroutine moduleName (Routine name args stmts exported varCount doEdgesCount callArgsMaxCount lineNumber endLineNumber)
  | not exported = return ()
  | otherwise = do
        functionAlias (fromString $ moduleName ++ "." ++ name) (fromString $ "." ++ moduleName ++ "." ++ name) [pFrameType] void

defineMain :: MetadataNodeID -> MetadataNodeID -> MetadataNodeID -> MetadataNodeID -> Routine -> ModuleBuilder ()
defineMain fileMetadataID compileUnitMetadataID nodePtrTypeMetadataID metadataNodeID routine@(Routine name args stmts exported vars doEdgesCount callArgsMaxCount lineNumber endLineNumber) = do
    function (fromString "main") [] void $ \ _ -> mdo
        (frame,varArray,doEdgesArray,callArgsArray,callArgsArraySizeof,exitLabel) <- functionPrologue metadataNodeID nodePtrTypeMetadataID routine (nullConst pFrameType) entryLabel False
        entryLabel <- block
        let callerCallArgsArray = nullConst pppNodeType
        let callerCallArgsCount = intConst 32 0
        mapM_ (\ var@(Var name index isCallArg) -> do
            varAddr <- gep varArray [intConst 32 index]
            call debugAddr (localVariableMetadata varAddr name 0 nodePtrTypeMetadataID metadataNodeID)
            ) [] -- TODO ... vars
        codeGenStmts metadataNodeID name frame varArray doEdgesArray callArgsArray callArgsArraySizeof callerCallArgsArray callerCallArgsCount exitLabel stmts
    subprogramMetadata fileMetadataID compileUnitMetadataID metadataNodeID name lineNumber
    return ()

routineName :: Bool -> String -> String -> Name
routineName exported mod rout =
  fromString $ (if exported then "" else ".") ++ mod ++ "." ++ rout

routineRef :: String -> Maybe String -> String -> Operand
routineRef mod maybeMod rout =
    functionRef (maybe (routineName False mod) (routineName True) maybeMod $ rout) [pFrameType] void

functionPrologue :: (MonadIRBuilder m, MonadFix m) => MetadataNodeID -> MetadataNodeID -> Routine -> Operand -> Name -> Bool -> m (Operand,Operand,Operand,Operand,Operand,Name)
functionPrologue subprogramMetadataID nodePtrTypeMetadataID (Routine name args stmts exported vars doEdgesCount callArgsMaxCount lineNumber endLineNumber) callerFrame entryLabel doCompactCall = mdo
    let varCount = fromIntegral $ length vars
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
    (callArgsArray,callArgsArraySizeof) <- if callArgsMaxCount == 0
      then do
        return (nullConst pppNodeType,intConst 32 0)
      else do
        callArgsArray <- alloca ppNodeType (Just (intConst 32 callArgsMaxCount)) 0
        callArgsArraySizeofPtr <- gep (nullConst pppNodeType) [intConst 32 callArgsMaxCount]
        callArgsArraySizeof <- ptrtoint callArgsArraySizeofPtr i32
        callArgsArrayRawPtr <- bitcast callArgsArray (ptr i8)
        call memset [callArgsArrayRawPtr,intConst 8 0,callArgsArraySizeof,intConst 32 0,intConst 1 0]
        return (callArgsArray,callArgsArraySizeof)
    callArgsArrayPtr <- gep frame [intConst 32 0,intConst 32 6]
    store callArgsArrayPtr 0 callArgsArray

    br entryLabel

    exitLabel <- block
    br doExitCleanupLabel `lineNumberMetadata` (subprogramMetadataID,endLineNumber)

    doExitCleanupLabel <- block
    mapM_ (\ doEdgesIndex -> mdo
        edgesArrayPtr <- gep doEdgesArray [intConst 32 doEdgesIndex,intConst 32 1]
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
    if doCompactCall
      then do
        call compact [callerFrame]
        return ()
      else do
        return ()
    retVoid

    return (frame,varArray,doEdgesArray,callArgsArray,callArgsArraySizeof,exitLabel)

codeGenStmts :: (MonadIRBuilder m, MonadFix m) => MetadataNodeID -> String -> Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> Operand -> Name -> [Statement] -> m ()
codeGenStmts subprogramMetadataID moduleName frame varArray doEdgesArray callArgsArray callArgsArraySizeof callerCallArgsArray callerCallArgsCount exitLabel stmts = do
    fellthru <- foldM (codeGenStmt []) True stmts
    when fellthru $ br exitLabel
  where
    codeGenStmt :: (MonadIRBuilder m, MonadFix m) => [(Integer,Name)] -> Bool -> Statement -> m Bool
    codeGenStmt _ False _ = do
        -- unreachable statements
        return False
    codeGenStmt _ _ (LetEq var val lineNumber) = do
        trace lineNumber "%d: LET=\n"
        varPtr <- getVarPtr var (Just lineNumber)
        node <- getVal val
        store varPtr 0 node
        return True
    codeGenStmt _ _ (LetAddEdge var val lineNumber) = do
        trace lineNumber "%d: LET>\n"
        varNode <- getVar var (Just lineNumber)
        node <- getVal val
        call addEdge [varNode,node]
        return True
    codeGenStmt _ _ (LetRemoveEdge (var0,var1) lineNumber) = do
        trace lineNumber "%d: LET<\n"
        node0 <- getVar var0 (Just lineNumber)
        node1 <- getVar var1 Nothing
        call removeEdge [node0,node1]
        return True
    codeGenStmt doStack _ (If ifBranches lineNumber endLineNumber) = mdo
        mapM_ (codeGenIfBranch doStack exitIfLabel) ifBranches
        br exitIfLabel `lineNumberMetadata` (subprogramMetadataID,endLineNumber)
        exitIfLabel <- block
        return True
    codeGenStmt _ _ (Call (maybeMod,rout) callArgs lineNumber) = do
        trace lineNumber "%d: CALL\n"
        callArgsArrayRawPtr <- bitcast callArgsArray (ptr i8)
        call memset [callArgsArrayRawPtr,intConst 8 0,callArgsArraySizeof,intConst 32 0,intConst 1 0] `lineNumberMetadata` (subprogramMetadataID,lineNumber)
        zipWithM_ storeCallArg [0..] callArgs
        call (routineRef moduleName maybeMod rout) [frame]
        return True
    codeGenStmt _ _ (Return lineNumber) = do
        trace lineNumber "%d: RETURN\n"
        br exitLabel `lineNumberMetadata` (subprogramMetadataID,lineNumber)
        -- workaround for br exitLabel not getting emitted for some reason
        bogusLabel <- block
        retVoid
        return False
    codeGenStmt doStack _ (DoLoop _ doIndex stmts lineNumber endLineNumber) = mdo
        trace lineNumber "%d: DO\n"
        br loopLabel `lineNumberMetadata` (subprogramMetadataID,lineNumber)

        loopLabel <- block
        fellthru <- foldM (codeGenStmt ((doIndex,exitLoopLabel):doStack)) True stmts
        when fellthru $ br loopLabel `lineNumberMetadata` (subprogramMetadataID,endLineNumber)

        exitLoopLabel <- block
        return $ any (hasExit doIndex) stmts
    codeGenStmt doStack _ (DoEdges (var0,var1) doIndex doEdgesIndex stmts lineNumber endLineNumber) = mdo
        trace lineNumber "%d: DO<\n"
        (var0Ptr,var1EdgesSize,edgesArray) <- setupDoEdges var0 var1 doEdgesIndex initLoopLabel exitLoopLabel lineNumber

        initLoopLabel <- block
        br loopLabel

        loopLabel <- block
        loopIndex <- phi [(intConst 32 0,initLoopLabel),(nextLoopIndex,loopIterateLabel)]
        loopIndexCheck <- icmp uge loopIndex var1EdgesSize
        condBr loopIndexCheck exitLoopLabel loopLabel2

        loopLabel2 <- block
        currentEdgePtr <- gep edgesArray [loopIndex]
        currentEdge <- load currentEdgePtr 0
        currentEdgeNullCheck <- icmp eq currentEdge (nullConst pNodeType)
        condBr currentEdgeNullCheck loopIterateLabel loopLabel3

        loopLabel3 <- block
        store var0Ptr 0 currentEdge
        store currentEdgePtr 0 (nullConst pNodeType)
        br enterLoopBodyLabel

        enterLoopBodyLabel <- block
        fellthru <- foldM (codeGenStmt ((doIndex,exitLoopLabel):doStack)) True stmts
        when fellthru $ br loopIterateLabel

        loopIterateLabel <- block
        nextLoopIndex <- add loopIndex (intConst 32 1)
        br loopLabel `lineNumberMetadata` (subprogramMetadataID,endLineNumber)

        exitLoopLabel <- block
        return True
    codeGenStmt doStack _ (Exit _ doIndex lineNumber) = do
        trace lineNumber "%d: EXIT\n"
        let exitDoLabel = maybe (error "INTERNAL ERROR: INVALID EXIT DOINDEX") id $ lookup doIndex doStack
        br exitDoLabel `lineNumberMetadata` (subprogramMetadataID,lineNumber)
        -- workaround for br exitDoLabel not getting emitted for some reason
        bogusLabel <- block
        retVoid
        return False

    codeGenIfBranch :: (MonadIRBuilder m, MonadFix m) => [(Integer,Name)] -> Name -> IfBranch -> m ()
    codeGenIfBranch doStack exitIfLabel (IfEq (var0,var1) stmts lineNumber) = mdo
        trace lineNumber "%d: IF=\n"
        node0 <- getVar var0 (Just lineNumber)
        node1 <- getVar var1 Nothing
        eqCheck <- icmp eq node0 node1
        condBr eqCheck enterBranchLabel nextBranchLabel

        enterBranchLabel <- block
        fellThru <- foldM (codeGenStmt doStack) True stmts
        when fellThru $ br exitIfLabel

        nextBranchLabel <- block
        return ()
    codeGenIfBranch doStack exitIfLabel (IfEdge (var0,var1) stmts lineNumber) = mdo
        trace lineNumber "%d: IF>\n"
        node0 <- getVar var0 (Just lineNumber)
        node1 <- getVar var1 Nothing
        edgeCheck <- call hasEdge [node0,node1]
        condBr edgeCheck enterBranchLabel nextBranchLabel

        enterBranchLabel <- block
        fellThru <- foldM (codeGenStmt doStack) True stmts
        when fellThru $ br exitIfLabel

        nextBranchLabel <- block
        return ()
    codeGenIfBranch doStack exitIfLabel (IfElse stmts lineNumber) = mdo
        trace lineNumber "%d: ELSE\n"
        br elseLabel `lineNumberMetadata` (subprogramMetadataID,lineNumber)

        elseLabel <- block
        fellThru <- foldM (codeGenStmt doStack) True stmts
        return ()

    storeCallArg :: (MonadIRBuilder m, MonadFix m) => Integer -> Val -> m ()
    storeCallArg index NewVal = do
        -- no action needed, already null
        return ()
    storeCallArg index (Val var) = mdo
        varPtr <- getVarPtr var Nothing
        argPtr <- gep callArgsArray [intConst 32 index]
        store argPtr 0 varPtr

    getVarPtr :: (MonadIRBuilder m, MonadFix m) => Var -> Maybe Integer -> m Operand
    getVarPtr (Var _ index isCallArg) maybeLineNumber
      | not isCallArg = do
            var <- gep varArray [intConst 32 index]
            maybe (return var) (lineNumberMetadata (return var) . (,) subprogramMetadataID) maybeLineNumber
      | otherwise = mdo
            callerCallArgsCountCheck <- icmp uge (intConst 32 index) callerCallArgsCount
            maybe (return ()) (lineNumberMetadata (return ()) . (,) subprogramMetadataID) maybeLineNumber
            condBr callerCallArgsCountCheck localPtrLabel checkCallerArgLabel

            checkCallerArgLabel <- block
            callerArgPtrPtr <- gep callerCallArgsArray [intConst 32 index]
            callerArgPtr <- load callerArgPtrPtr 0
            callerArgPtrCheck <- icmp eq callerArgPtr (nullConst ppNodeType)
            condBr callerArgPtrCheck localPtrLabel resultLabel

            localPtrLabel <- block
            localPtr <- gep varArray [intConst 32 index]
            br resultLabel

            resultLabel <- block
            phi [(localPtr,localPtrLabel),(callerArgPtr,checkCallerArgLabel)]

    getVar :: (MonadIRBuilder m, MonadFix m) => Var -> Maybe Integer -> m Operand
    getVar var maybeLineNumber = mdo
        br getVarLabel

        getVarLabel <- block
        varPtr <- getVarPtr var maybeLineNumber
        br getVarNullCheckLabel

        getVarNullCheckLabel <- block
        node <- load varPtr 0
        nullCheck <- icmp eq node (nullConst pNodeType)
        condBr nullCheck newNodeLabel doneLabel

        newNodeLabel <- block
        n <- call newNode [frame]
        store varPtr 0 n
        br doneLabel

        doneLabel <- block
        phi [(node,getVarNullCheckLabel),(n,newNodeLabel)]

    getVal :: (MonadIRBuilder m, MonadFix m) => Val -> m Operand
    getVal NewVal = call newNode [frame]
    getVal (Val var) = getVar var Nothing

    setupDoEdges :: (MonadIRBuilder m, MonadFix m) => Var -> Var -> Integer -> Name -> Name -> Integer -> m (Operand,Operand,Operand)
    setupDoEdges var0 var1 doEdgesIndex initLoopLabel exitLoopLabel lineNumber = mdo
        var0Ptr <- getVarPtr var0 (Just lineNumber)
        node1 <- getVar var1 Nothing
        node1EdgesSizePtr <- gep node1 [intConst 32 0,intConst 32 2]
        node1EdgesSize <- load node1EdgesSizePtr 0
        node1EdgesSizeofPtr <- gep (nullConst ppNodeType) [node1EdgesSize]
        node1EdgesSizeof <- ptrtoint node1EdgesSizeofPtr i32
        node1EdgesSizeCheck <- icmp eq node1EdgesSize (intConst 32 0)
        condBr node1EdgesSizeCheck exitLoopLabel setupEdgesLabel

        setupEdgesLabel <- block
        edgesArraySizePtr <- gep doEdgesArray [intConst 32 doEdgesIndex,intConst 32 0]
        -- workaround for
        -- *** Exception: gep: Can't index into a NamedTypeReference (Name "doEdgesIterator")
        edgesArraySize_workaround <- load edgesArraySizePtr 0
        edgesArraySize <- bitcast edgesArraySize_workaround i32
        edgesArrayPtr <- gep doEdgesArray [intConst 32 doEdgesIndex,intConst 32 1]
        edgesArraySize0Check <- icmp eq edgesArraySize (intConst 32 0)
        condBr edgesArraySize0Check allocEdgesArrayLabel checkEdgesArraySizeLabel

        checkEdgesArraySizeLabel <- block
        edgesArraySizeCheck <- icmp uge edgesArraySize node1EdgesSize
        condBr edgesArraySizeCheck clearEdgesArrayLabel freeOldEdgesArrayLabel

        clearEdgesArrayLabel <- block
        -- workaround for
        -- *** Exception: gep: Can't index into a NamedTypeReference (Name "doEdgesIterator")
        reusedEdgesArray_workaround <- load edgesArrayPtr 0
        reusedEdgesArray <- bitcast reusedEdgesArray_workaround ppNodeType
        reusedEdgesArrayRawPtr <- bitcast reusedEdgesArray (ptr i8)
        reusedEdgesArraySizeofPtr <- gep (nullConst ppNodeType) [edgesArraySize]
        reusedEdgesArraySizeof <- ptrtoint reusedEdgesArraySizeofPtr i32
        call memset [reusedEdgesArrayRawPtr,intConst 8 0,reusedEdgesArraySizeof,intConst 32 0,intConst 1 0]
        br copyEdgesLabel

        freeOldEdgesArrayLabel <- block
        -- workaround for
        -- *** Exception: gep: Can't index into a NamedTypeReference (Name "doEdgesIterator")
        oldEdgesArray_workaround <- load edgesArrayPtr 0
        oldEdgesArray <- bitcast oldEdgesArray_workaround ppNodeType
        oldEdgesArrayRawPtr <- bitcast oldEdgesArray (ptr i8)
        call free [oldEdgesArrayRawPtr]
        br allocEdgesArrayLabel

        allocEdgesArrayLabel <- block
        newEdgesArrayRawPtr <- call malloc [node1EdgesSizeof]
        newEdgesArray <- bitcast newEdgesArrayRawPtr ppNodeType
        store edgesArrayPtr 0 newEdgesArray
        store edgesArraySizePtr 0 node1EdgesSize
        br copyEdgesLabel

        copyEdgesLabel <- block
        edgesArrayRawPtr <- phi [
            (reusedEdgesArrayRawPtr,clearEdgesArrayLabel),
            (newEdgesArrayRawPtr,allocEdgesArrayLabel)
            ]
        edgesArray <- phi [
            (reusedEdgesArray,clearEdgesArrayLabel),
            (newEdgesArray,allocEdgesArrayLabel)
            ]
        node1EdgesArrayPtr <- gep node1 [intConst 32 0,intConst 32 3]
        node1EdgesArray <- load node1EdgesArrayPtr 0
        node1EdgesArrayRawPtr <- bitcast node1EdgesArray (ptr i8)
        call memcpy [edgesArrayRawPtr,node1EdgesArrayRawPtr,node1EdgesSizeof,intConst 32 0,intConst 1 0]
        br initLoopLabel
        return (var0Ptr,node1EdgesSize,edgesArray)

hasExit :: Integer -> Statement -> Bool
hasExit doIndex (If ifBranches _ _) = any (any (hasExit doIndex) . ifBranchStmts) ifBranches
hasExit doIndex (Exit _ exitDoIndex _) = doIndex == exitDoIndex
hasExit doIndex (DoLoop _ _ stmts _ _) = any (hasExit doIndex) stmts
hasExit doIndex (DoEdges _ _ _ stmts _ _) = any (hasExit doIndex) stmts
hasExit _ _ = False

debugTypeMetadata :: MetadataNodeID -> ModuleBuilder (MetadataNodeID,MetadataNodeID)
debugTypeMetadata (MetadataNodeID nodeIDNum) = do
    let nodeTypeMetadataID = MetadataNodeID nodeIDNum
    let nodePtrTypeMetadataID = MetadataNodeID (nodeIDNum + 1)
    emitDefn $ MetadataNodeDefinition nodeTypeMetadataID $ map Just [
        MDString (fromString "DICompositeType"),
        MDString (fromString "tag"),
        MDString (fromString "DW_TAG_structure_type"),
        MDString (fromString "name"),
        MDString (fromString "NODE"),
        MDString (fromString "elements"),
        MDNode $ MetadataNode $ map Just [
            MDNode $ MetadataNode $ map Just [
                MDString (fromString "DIBasicType"),
                MDString (fromString "size"),
                MDString (fromString "8"),
                MDString (fromString "align"),
                MDString (fromString "16"),
                MDString (fromString "encoding"),
                MDString (fromString "DW_ATE_unsigned_char")
                ],
            MDNode $ MetadataNode $ map Just [
                MDString (fromString "DIBasicType"),
                MDString (fromString "size"),
                MDString (fromString "1"),
                MDString (fromString "align"),
                MDString (fromString "16"),
                MDString (fromString "encoding"),
                MDString (fromString "DW_ATE_boolean")
                ],
            MDNode $ MetadataNode $ map Just [
                MDString (fromString "DIBasicType"),
                MDString (fromString "size"),
                MDString (fromString "32"),
                MDString (fromString "align"),
                MDString (fromString "32"),
                MDString (fromString "encoding"),
                MDString (fromString "DW_ATE_boolean")
                ],
            MDNode $ MetadataNode $ map Just [
                MDString (fromString "DIDerivedType"),
                MDString (fromString "size"),
                MDString (fromString "64"),
                MDString (fromString "align"),
                MDString (fromString "64"),
                MDString (fromString "tag"),
                MDString (fromString "DW_TAG_pointer_type"),
                MDString (fromString "baseType"),
                MDNode (MetadataNodeReference nodePtrTypeMetadataID)
                ]
            ]
        ]
    emitDefn $ MetadataNodeDefinition nodePtrTypeMetadataID $ map Just [
        MDString (fromString "DIDerivedType"),
        MDString (fromString "size"),
        MDString (fromString "64"),
        MDString (fromString "align"),
        MDString (fromString "64"),
        MDString (fromString "tag"),
        MDString (fromString "DW_TAG_pointer_type"),
        MDString (fromString "baseType"),
        MDNode (MetadataNodeReference nodeTypeMetadataID)
        ]
    return (nodePtrTypeMetadataID,MetadataNodeID (nodeIDNum + 2))
