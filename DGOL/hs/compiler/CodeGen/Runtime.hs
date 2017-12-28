{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.Runtime(
    runtimeDecls,
    runtimeDefs,
    memset,memcpy,malloc,free,
    newNode,hasEdge,addEdge,removeEdge
)
where

import LLVM.AST(Definition(GlobalDefinition))
import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,memberValues,integerBits,integerValue,isPacked,structName,memberType)
import LLVM.AST.Global(initializer,name,type',globalVariableDefaults)
import LLVM.AST.Name(Name)
import LLVM.AST.Operand(Operand)
import LLVM.AST.Type(Type(StructureType),void,i1,i8,i32,elementTypes,ptr)
import qualified LLVM.AST.Type
import LLVM.IRBuilder.Instruction(add,bitcast,br,condBr,gep,icmp,load,phi,ptrtoint,ret,retVoid,store,sub)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),emitDefn,extern,function)
import LLVM.IRBuilder.Monad(emitInstr,block)

import CodeGen.Types(
    nodeTypeName,nodeType,pNodeType,ppNodeType,nodeTypedef,
    edgeArrayIncrement,pageSize,newPageThreshold,
    pageTypeName,pageType,pPageType,pageTypedef,
    pFrameType,frameTypedef,
    doEdgesIteratorType,pDoEdgesIteratorType,doEdgesIteratorTypedef)
import CodeGen.Util(
    eq,uge,
    intConst,nullConst,
    functionRef,globalRef,
    call)

newNodeName :: Name
newNodeName = "newNode"

newNode :: Operand
newNode = functionRef newNodeName [pFrameType] pNodeType

newNodeDecl :: ModuleBuilder Operand
newNodeDecl = extern newNodeName [pFrameType] pNodeType

newNodeImpl :: ModuleBuilder Operand
newNodeImpl = function newNodeName [(pFrameType,NoParameterName)] pNodeType (\ [topFrame] -> mdo
    -- look for dead entries in the tables
    -- if none, garbage collect, then if last page is more than X% full,
    -- allocate a new last page
    -- then retry looking for dead entries
    br retryNewNode

    retryNewNode <- block
    initialPage <- gep globalState [intConst 32 0, intConst 32 1]
    br newNodePageLoop

    -- iterate over pages, for each page
    --   iterate over page.nodes, for each node
    --     if node.alive is 0
    --        set node.alive = 1
    --        return &node
    newNodePageLoop <- block
    newNodePage <- phi [
        (initialPage,retryNewNode),
        (newNodePage,newNodePageLoopNextIndex),
        (newNodeNextPage,newNodePageLoopNextPage)
        ]
    newNodeIndex <- phi [
        (intConst 32 0,retryNewNode),
        (newNodeNextIndex,newNodePageLoopNextIndex),
        (intConst 32 0,newNodePageLoopNextPage)
        ]
    newNodeAlivePtr <- gep newNodePage [intConst 32 0, intConst 32 1, newNodeIndex, intConst 32 1]
    newNodeAlive <- load newNodeAlivePtr 0
    condBr newNodeAlive newNodePageLoopNextIndex returnNewNode

    -- found free node
    returnNewNode <- block
    store newNodeAlivePtr 0 (intConst 1 1)
    newNodePtr <- gep newNodePage [intConst 32 0, intConst 32 1, newNodeIndex]
    ret newNodePtr

    -- iterate to next node in page
    newNodePageLoopNextIndex <- block
    newNodeNextIndex <- add newNodeIndex (intConst 32 1)
    newNodeIndexRangeCheck <- icmp uge newNodeNextIndex (intConst 32 pageSize)
    condBr newNodeIndexRangeCheck newNodePageLoopNextPage newNodePageLoop

    -- iterate to next page
    newNodePageLoopNextPage <- block
    newNodeNextPagePtr <- gep newNodePage [intConst 32 0, intConst 32 0]
    newNodeNextPage <- load newNodeNextPagePtr 0
    newNodePageNullCheck <- icmp eq newNodeNextPage (nullConst pPageType)
    condBr newNodePageNullCheck startGCMark newNodePageLoop

    -- mark
    -- iterate over frames, for each frame
    --   iterate over vars, for each var
    --     call gcMark
    --   iterate over doEdgesIterators, for each doEdgesIterator
    --     iterate from iterator index to size of allocated array of edges
    --       call gcMark
    startGCMark <- block
    gcMarkPtr <- gep globalState [intConst 32 0, intConst 32 0]
    oldGcMark <- load gcMarkPtr 0
    newGcMark <- add oldGcMark (intConst 8 1)
    store gcMarkPtr 0 newGcMark
    br markFrameLoop

    -- iterate over frames
    markFrameLoop <- block
    frame <- phi [
        (topFrame,startGCMark),
        (markNextFrame,markDoEdgesIteratorsLoop)
        ]
    frameCheck <- icmp eq frame (nullConst pFrameType)
    condBr frameCheck startGCSweep markFrameLoopBody

    markFrameLoopBody <- block
    markNextFramePtr <- gep frame [intConst 32 0, intConst 32 0]
    markNextFrame <- load markNextFramePtr 0
    markVarArraySizePtr <- gep frame [intConst 32 0, intConst 32 1]
    markVarArraySize <- load markVarArraySizePtr 0
    markVarArrayPtr <- gep frame [intConst 32 0, intConst 32 2]
    markVarArray <- load markVarArrayPtr 0
    markDoEdgesIteratorsSizePtr <- gep frame [intConst 32 0, intConst 32 3]
    markDoEdgesIteratorsSize <- load markDoEdgesIteratorsSizePtr 0
    markDoEdgesIteratorsArrayPtr <- gep frame [intConst 32 0, intConst 32 4]
    markDoEdgesIteratorsArray <- load markDoEdgesIteratorsArrayPtr 0
    br markVarLoop

    -- iterate over vars in frame
    markVarLoop <- block
    markVarIndex <- phi [
        (intConst 32 0,markFrameLoopBody),
        (markVarNextIndex,markVarLoopBody)
        ]
    markVarNextIndex <- add markVarIndex (intConst 32 1)
    markVarIndexCheck <- icmp uge markVarIndex markVarArraySize
    condBr markVarIndexCheck markDoEdgesIteratorsLoop markVarLoopBody

    -- mark var
    markVarLoopBody <- block
    markVarNodePtr <- gep markVarArray [markVarIndex]
    markVarNode <- load markVarNodePtr 0
    call gcMarkNode [newGcMark,markVarNode]
    br markVarLoop

    -- iterate over doEdgeIterators in frame
    markDoEdgesIteratorsLoop <- block
    markDoEdgesIteratorsIndex <- phi [
        (intConst 32 0,markVarLoop),
        (markDoEdgesIteratorsNextIndex,markIteratorEdgesLoop)
        ]
    markDoEdgesIteratorsNextIndex <- add markDoEdgesIteratorsIndex (intConst 32 1)
    markDoEdgesIteratorsIndexCheck <- icmp uge markDoEdgesIteratorsIndex markDoEdgesIteratorsSize
    condBr markDoEdgesIteratorsIndexCheck markFrameLoop markDoEdgesIteratorLoopBody

    -- set up current doEdgeIterator
    markDoEdgesIteratorLoopBody <- block
    markIteratorEdgesSizePtr <- gep markDoEdgesIteratorsArray [markDoEdgesIteratorsIndex, intConst 32 0]
    markIteratorEdgesSize <- load markIteratorEdgesSizePtr 0
    markIteratorEdgesArrayPtr <- gep markDoEdgesIteratorsArray [markDoEdgesIteratorsIndex, intConst 32 1]
    markIteratorEdgesArray <- load markIteratorEdgesArrayPtr 0
    br markIteratorEdgesLoop

    -- iterate over edges in current doEdgeIterator
    markIteratorEdgesLoop <- block
    markIteratorEdgesIndex <- phi [
        (intConst 32 0,markDoEdgesIteratorLoopBody),
        (markIteratorEdgesNextIndex,markIteratorEdgesLoopBody)
        ]
    markIteratorEdgesNextIndex <- add markIteratorEdgesIndex (intConst 32 1)
    markIteratorEdgesIndexCheck <- icmp uge markIteratorEdgesIndex markIteratorEdgesSize
    condBr markIteratorEdgesIndexCheck markDoEdgesIteratorsLoop markIteratorEdgesLoopBody

    -- mark edge
    markIteratorEdgesLoopBody <- block
    iteratorEdgePtr <- gep markIteratorEdgesArray [markIteratorEdgesIndex]
    iteratorEdge <- load iteratorEdgePtr 0
    call gcMarkNode [newGcMark,iteratorEdge]
    br markIteratorEdgesLoop

    -- sweep
    -- iterate over pages, for each page
    --   reset pageLiveCount to 0
    --   iterate over page.nodes, for each node
    --     if node.alive is 1
    --       if node.gcMark is not newGcMark
    --         set node.alive to 0
    --         if node.edgeArraySize > 0
    --           clear node.edgeArray
    --       else
    --         increment pageLiveCount
    -- if on last page,
    --   if pageLiveCount >= newPageThreshold,
    --     lastPage.nextPage = allocate and clear new page
    startGCSweep <- block
    br sweepPageLoop

    sweepPageLoop <- block
    sweepPage <- phi [
        (initialPage,startGCSweep),
        (sweepPage,sweepPageLoopNextIndex),
        (sweepNextPage,sweepPageLoopNextPage)
        ]
    sweepIndex <- phi [
        (intConst 32 0,startGCSweep),
        (sweepNextIndex,sweepPageLoopNextIndex),
        (intConst 32 0,sweepPageLoopNextPage)
        ]
    sweepPageLiveCount <- phi [
        (intConst 32 0,startGCSweep),
        (sweepNextPageLiveCount,sweepPageLoopNextIndex),
        (intConst 32 0,sweepPageLoopNextPage)
        ]
    sweepNodeAlivePtr <- gep sweepPage [intConst 32 0, intConst 32 1, sweepIndex, intConst 32 1]
    sweepNodeAlive <- load sweepNodeAlivePtr 0
    condBr sweepNodeAlive sweepCheckNode sweepPageLoopNextIndex

    sweepCheckNode <- block
    sweepGcMarkPtr <- gep sweepPage [intConst 32 0, intConst 32 1, sweepIndex, intConst 32 0]
    sweepGcMark <- load sweepGcMarkPtr 0
    sweepGcMarkCheck <- icmp eq newGcMark sweepGcMark
    incrementedPageLiveCount <- add sweepPageLiveCount (intConst 32 1)
    condBr sweepGcMarkCheck sweepPageLoopNextIndex sweepCollectNode

    sweepCollectNode <- block
    store sweepNodeAlivePtr 0 (intConst 1 0)
    sweepNodeEdgesSizePtr <- gep sweepPage [intConst 32 0, intConst 32 1, sweepIndex, intConst 32 2]
    sweepNodeEdgesSize <- load sweepNodeEdgesSizePtr 0
    sweepNodeEdgesSizeCheck <- icmp eq sweepNodeEdgesSize (intConst 32 0)
    condBr sweepNodeEdgesSizeCheck sweepPageLoopNextIndex sweepCollectNodeClearEdges

    sweepCollectNodeClearEdges <- block
    sweepNodeEdgesArrayPtr <- gep sweepPage [intConst 32 0, intConst 32 1, sweepIndex, intConst 32 3]
    sweepNodeEdgesArray <- load sweepNodeEdgesArrayPtr 0
    sweepNodeEdgesRawPtr <- bitcast sweepNodeEdgesArray (ptr i8)
    sweepNodeEdgesSizeofPtr <- gep (nullConst pNodeType) [sweepNodeEdgesSize]
    sweepNodeEdgesSizeof <- ptrtoint sweepNodeEdgesSizeofPtr i32
    call memset [sweepNodeEdgesRawPtr,intConst 8 0,sweepNodeEdgesSizeof,intConst 32 0,intConst 1 0]
    br sweepPageLoopNextIndex

    sweepPageLoopNextIndex <- block
    sweepNextPageLiveCount <- phi [
        (sweepPageLiveCount,sweepPageLoop),
        (incrementedPageLiveCount,sweepCheckNode),
        (sweepPageLiveCount,sweepCollectNode),
        (sweepPageLiveCount,sweepCollectNodeClearEdges)
        ]
    sweepNextIndex <- add sweepIndex (intConst 32 1)
    sweepIndexRangeCheck <- icmp uge sweepNextIndex (intConst 32 pageSize)
    condBr sweepIndexRangeCheck sweepPageLoopNextPage sweepPageLoop

    sweepPageLoopNextPage <- block
    sweepNextPagePtr <- gep sweepPage [intConst 32 0, intConst 32 0]
    sweepNextPage <- load sweepNextPagePtr 0
    sweepPageNullCheck <- icmp eq sweepNextPage (nullConst pPageType)
    condBr newNodePageNullCheck checkForNewPage sweepPageLoop

    checkForNewPage <- block
    newPageCheck <- icmp uge sweepNextPageLiveCount (intConst 32 newPageThreshold)
    condBr newPageCheck newPage retryNewNode

    newPage <- block
    newPageSizeofPtr <- gep (nullConst pPageType) [intConst 32 1]
    newPageSizeof <- ptrtoint newPageSizeofPtr i32
    newPageRawPtr <- call malloc [newPageSizeof]
    call memset [newPageRawPtr,intConst 8 0,newPageSizeof,intConst 32 0,intConst 1 0]
    newPagePtr <- bitcast newPageRawPtr pPageType
    store sweepNextPagePtr 0 newPagePtr
    br retryNewNode
    )

globalStateName :: Name
globalStateName = "globalState"

globalStateType :: Type
globalStateType = StructureType {
    LLVM.AST.Type.isPacked = False,
    elementTypes = [
        i8, -- gc mark
        pageType -- root page
        ]
    }

globalState :: Operand
globalState = globalRef globalStateName globalStateType

globalStateDef :: ModuleBuilder ()
globalStateDef = do
    emitDefn (GlobalDefinition globalVariableDefaults {
        name = globalStateName,
        type' = globalStateType,
        initializer = Just (Struct { -- zeroinitializer would have been nice
            structName = Nothing,
            isPacked = False,
            memberValues = [
                Int { integerBits = 8, integerValue = 0 },
                Struct {
                    structName = Just pageTypeName,
                    isPacked = False,
                    memberValues = [
                        Null { constantType = pPageType },
                        Array {
                            memberType = nodeType,
                            memberValues = replicate pageSize Struct {
                                structName = Just nodeTypeName,
                                isPacked = False,
                                memberValues = [
                                    Int { integerBits = 8, integerValue = 0 },
                                    Int { integerBits = 1, integerValue = 0 },
                                    Int { integerBits = 64, integerValue = 0 },
                                    Null { constantType = ppNodeType }
                                    ]
                                }
                            }
                        ]
                    }
                ]
            })
        })

gcMarkNodeName :: Name
gcMarkNodeName = "gcMarkNode"

gcMarkNode :: Operand
gcMarkNode = functionRef gcMarkNodeName [i8,pNodeType] void

gcMarkNodeImpl :: ModuleBuilder Operand
gcMarkNodeImpl = function gcMarkNodeName [(i8,NoParameterName),(pNodeType,NoParameterName)] void $ \ [gcMark,pNode] -> mdo
        -- if node is null, return
        nullCheck <- icmp eq pNode (nullConst pNodeType)
        condBr nullCheck done checkAlive

        -- if not node.alive, return
        checkAlive <- block
        alivePtr <- gep pNode [intConst 32 0, intConst 32 1]
        aliveCheck <- load alivePtr 0
        condBr aliveCheck checkGcMark done

        -- if node.gcMark = gcMark, return
        checkGcMark <- block
        nodeGcMarkPtr <- gep pNode [intConst 32 0, intConst 32 0]
        nodeGcMark <- load nodeGcMarkPtr 0
        nodeGcMarkCheck <- icmp eq gcMark nodeGcMark
        condBr nodeGcMarkCheck done checkEdgesInit

        checkEdgesInit <- block
        store nodeGcMarkPtr 0 gcMark
        nodeEdgesArraySizePtr <- gep pNode [intConst 32 0, intConst 32 2]
        nodeEdgesArraySize <- load nodeEdgesArraySizePtr 0
        nodeEdgesArrayPtr <- gep pNode [intConst 32 0, intConst 32 3]
        nodeEdgesArray <- load nodeEdgesArrayPtr 0
        br checkEdgesLoop

        checkEdgesLoop <- block
        edgeIndex <- phi [
            (intConst 32 0,checkEdgesInit),
            (nextEdgeIndex,checkEdgesLoopBody)
            ]
        -- if edgeIndex >= node.edgeArraySize, return
        edgeIndexCheck <- icmp uge edgeIndex nodeEdgesArraySize
        condBr edgeIndexCheck done checkEdgesLoopBody
        
        checkEdgesLoopBody <- block
        edgeElementPtr <- gep nodeEdgesArray [edgeIndex]
        edgeElement <- load edgeElementPtr 0
        call gcMarkNode [gcMark,edgeElement]
        nextEdgeIndex <- add edgeIndex (intConst 32 1)
        br checkEdgesLoop

        done <- block
        retVoid

hasEdgeName :: Name
hasEdgeName = "hasEdge"

hasEdge :: Operand
hasEdge = functionRef hasEdgeName [pNodeType,pNodeType] i1

hasEdgeDecl :: ModuleBuilder Operand
hasEdgeDecl = extern hasEdgeName [pNodeType,pNodeType] i1

hasEdgeImpl :: ModuleBuilder Operand
hasEdgeImpl = function hasEdgeName [(pNodeType,NoParameterName),(pNodeType,NoParameterName)] i1 $ \ [node,edge] -> mdo
    entry <- block
    edgeArraySizePtr <- gep node [intConst 32 0,intConst 32 2]
    edgeArraySize <- load edgeArraySizePtr 0
    edgeArrayPtr <- gep node [intConst 32 0,intConst 32 3]
    edgeArray <- load edgeArrayPtr 0
    br edgeLoop

    edgeLoop <- block
    edgeIndex <- phi [
        (intConst 32 0,entry),
        (edgeNextIndex,checkCurrentEdge)
        ]
    edgeNextIndex <- add edgeIndex (intConst 32 1)
    edgeIndexCheck <- icmp uge edgeIndex edgeArraySize
    condBr edgeIndexCheck retFalse checkCurrentEdge

    checkCurrentEdge <- block
    currentEdgePtr <- gep edgeArray [edgeIndex]
    currentEdge <- load currentEdgePtr 0
    currentEdgeCheck <- icmp eq currentEdge edge
    condBr currentEdgeCheck retTrue edgeLoop

    retTrue <- block
    ret (intConst 1 1)

    retFalse <- block
    ret (intConst 1 0)

addEdgeName :: Name
addEdgeName = "addEdge"

addEdge :: Operand
addEdge = functionRef addEdgeName [pNodeType,pNodeType] void

addEdgeDecl :: ModuleBuilder Operand
addEdgeDecl = extern addEdgeName [pNodeType,pNodeType] void

addEdgeImpl :: ModuleBuilder Operand
addEdgeImpl = function addEdgeName [(pNodeType,NoParameterName),(pNodeType,NoParameterName)] void $ \ [node,edge] -> mdo
    alreadyHasEdge <- call hasEdge [node,edge]
    condBr alreadyHasEdge done initLoop

    initLoop <- block
    edgeArraySizePtr <- gep node [intConst 32 0,intConst 32 2]
    -- workaround for
    -- *** Exception: gep: Can't index into a NamedTypeReference (Name "node")
    initEdgeArraySize_workaround <- load edgeArraySizePtr 0
    initEdgeArraySize <- bitcast initEdgeArraySize_workaround i32
    edgeArrayPtr <- gep node [intConst 32 0,intConst 32 3]
    -- workaround for
    -- *** Exception: gep: Can't index into a NamedTypeReference (Name "node")
    initEdgeArray_workaround <- load edgeArrayPtr 0
    initEdgeArray <- bitcast initEdgeArray_workaround ppNodeType
    br loop

    loop <- block
    edgeArraySize <- phi [
        (initEdgeArraySize,initLoop),
        (edgeArraySize,checkCurrentEdge),
        (newEdgeArraySize,reallocEdgeArray)
        ]
    edgeArray <- phi [
        (initEdgeArray,initLoop),
        (edgeArray,checkCurrentEdge),
        (newEdgeArray,reallocEdgeArray)
        ]
    edgeIndex <- phi [
        (intConst 32 0,initLoop),
        (edgeNextIndex,checkCurrentEdge),
        (edgeArraySize,reallocEdgeArray)
        ]
    edgeNextIndex <- add edgeIndex (intConst 32 1)
    edgeIndexCheck <- icmp uge edgeIndex edgeArraySize
    condBr edgeIndexCheck reallocEdgeArray checkCurrentEdge

    checkCurrentEdge <- block
    currentEdgePtr <- gep edgeArray [edgeIndex]
    currentEdge <- load currentEdgePtr 0
    currentEdgeCheck <- icmp eq currentEdge (nullConst pNodeType)
    condBr currentEdgeCheck foundFreeSlot loop

    foundFreeSlot <- block
    store currentEdgePtr 0 edge
    br done

    reallocEdgeArray <- block
    newEdgeArraySize <- add edgeArraySize (intConst 32 edgeArrayIncrement)
    newEdgeArrayAllocSizePtr <- gep (nullConst ppNodeType) [newEdgeArraySize]
    newEdgeArrayAllocSize <- ptrtoint newEdgeArrayAllocSizePtr i32
    newEdgeArrayRawPtr <- call malloc [newEdgeArrayAllocSize]
    oldEdgeArrayRawPtr <- bitcast edgeArray (ptr i8)
    oldEdgeArrayAllocSizePtr <- gep (nullConst ppNodeType) [edgeArraySize]
    oldEdgeArrayAllocSize <- ptrtoint oldEdgeArrayAllocSizePtr i32
    call memcpy [newEdgeArrayRawPtr,oldEdgeArrayRawPtr,oldEdgeArrayAllocSize,intConst 32 0,intConst 1 0]
    edgeArrayIncrementSizePtr <- gep (nullConst ppNodeType) [intConst 32 edgeArrayIncrement]
    edgeArrayIncrementSize <- ptrtoint edgeArrayIncrementSizePtr i32
    edgeArrayIncrementArray <- gep newEdgeArrayRawPtr [oldEdgeArrayAllocSize]
    call memset [edgeArrayIncrementArray,intConst 8 0,edgeArrayIncrementSize,intConst 32 0,intConst 1 0]
    store edgeArraySizePtr 0 newEdgeArraySize
    newEdgeArray <- bitcast newEdgeArrayRawPtr ppNodeType
    store edgeArrayPtr 0 newEdgeArray
    br loop

    done <- block
    retVoid

removeEdgeName :: Name
removeEdgeName = "removeEdge"

removeEdge :: Operand
removeEdge = functionRef removeEdgeName [pNodeType,pNodeType] void

removeEdgeDecl :: ModuleBuilder Operand
removeEdgeDecl = extern removeEdgeName [pNodeType,pNodeType] void

removeEdgeImpl :: ModuleBuilder Operand
removeEdgeImpl = function removeEdgeName [(pNodeType,NoParameterName),(pNodeType,NoParameterName)] void $ \ [node,edge] -> mdo
    entry <- block
    edgeArraySizePtr <- gep node [intConst 32 0,intConst 32 2]
    edgeArraySize <- load edgeArraySizePtr 0
    edgeArrayPtr <- gep node [intConst 32 0,intConst 32 3]
    edgeArray <- load edgeArrayPtr 0
    br edgeLoop

    edgeLoop <- block
    edgeIndex <- phi [
        (intConst 32 0,entry),
        (edgeNextIndex,checkCurrentEdge)
        ]
    edgeNextIndex <- add edgeIndex (intConst 32 1)
    edgeIndexCheck <- icmp uge edgeIndex edgeArraySize
    condBr edgeIndexCheck done checkCurrentEdge

    checkCurrentEdge <- block
    currentEdgePtr <- gep edgeArray [edgeIndex]
    currentEdge <- load currentEdgePtr 0
    currentEdgeCheck <- icmp eq currentEdge edge
    condBr currentEdgeCheck remove edgeLoop

    remove <- block
    store currentEdgePtr 0 (nullConst pNodeType)
    br done

    done <- block
    retVoid

memsetName :: Name
memsetName = "llvm.memset.p0i8.i32"

memset :: Operand
memset = functionRef memsetName [ptr i8,i8,i32,i32,i1] void

memsetDecl :: ModuleBuilder Operand
memsetDecl = extern memsetName [ptr i8,i8,i32,i32,i1] void

memcpyName :: Name
memcpyName = "llvm.memcpy.p0i8.i32"

memcpy :: Operand
memcpy = functionRef memcpyName [ptr i8,ptr i8,i32,i32,i1] void

memcpyDecl :: ModuleBuilder Operand
memcpyDecl = extern memcpyName [ptr i8,ptr i8,i32,i32,i1] void

mallocName :: Name
mallocName = "malloc"

malloc :: Operand
malloc = functionRef mallocName [i32] (ptr i8)

mallocDecl :: ModuleBuilder Operand
mallocDecl = extern mallocName [i32] (ptr i8)

freeName :: Name
freeName = "free"

free :: Operand
free = functionRef freeName [ptr i8] void

freeDecl :: ModuleBuilder Operand
freeDecl = extern freeName [ptr i8] void

commonDecls :: ModuleBuilder ()
commonDecls = do
    memsetDecl
    memcpyDecl
    mallocDecl
    freeDecl
    nodeTypedef
    pageTypedef
    frameTypedef
    doEdgesIteratorTypedef
    return ()

runtimeDefs :: ModuleBuilder ()
runtimeDefs = do
    commonDecls
    globalStateDef
    gcMarkNodeImpl
    newNodeImpl
    hasEdgeImpl
    addEdgeImpl
    removeEdgeImpl
    return ()

runtimeDecls :: ModuleBuilder ()
runtimeDecls = do
    commonDecls
    newNodeDecl
    hasEdgeDecl
    addEdgeDecl
    removeEdgeDecl
    return ()
