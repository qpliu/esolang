{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module CodeGen.Runtime(
    runtimeDecls,
    runtimeDefs,
    memset,memcpy,malloc,free,
    newNode,hasEdge,addEdge,removeEdge,compact,
    trace,traceLabel,tracef,traceEnabled,
    globalState
)
where

import Control.Monad.Fix(MonadFix)
import Data.String(fromString)

import LLVM.AST(Definition(GlobalDefinition))
import LLVM.AST.Constant(Constant(Array,GlobalReference,Int,Null,Struct),constantType,memberValues,integerBits,integerValue,isPacked,structName,memberType)
import LLVM.AST.Global(initializer,name,type',globalVariableDefaults)
import LLVM.AST.Name(Name(UnName))
import LLVM.AST.Operand(Operand)
import LLVM.AST.Type(Type(StructureType),void,i1,i8,i32,elementTypes,ptr)
import qualified LLVM.AST.Type
import LLVM.IRBuilder.Instruction(add,alloca,bitcast,br,condBr,gep,icmp,load,phi,ptrtoint,ret,retVoid,store,sub)
import LLVM.IRBuilder.Module(ModuleBuilder,ParameterName(NoParameterName),emitDefn,extern,function)
import LLVM.IRBuilder.Monad(MonadIRBuilder,emitInstr,block)

import CodeGen.Types(
    nodeTypeName,nodeType,pNodeType,ppNodeType,nodeTypedef,
    edgeArrayIncrement,pageSize,newPageThreshold,pageCompactionThreshold,
    pageTypeName,pageType,pPageType,pageTypedef,
    pFrameType,frameTypedef,
    doEdgesIteratorType,pDoEdgesIteratorType,doEdgesIteratorTypedef)
import CodeGen.Util(
    eq,uge,
    intConst,nullConst,
    functionRef,globalRef,varArgsFunctionRef,varArgsExtern,
    GlobalStrzTable(GlobalStrzTable),globalStrzAsI8Ptr,globalStrzDefs,
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
    compactionEligibleCount <- phi [
        (intConst 32 0,startGCSweep),
        (compactionEligibleCount,sweepPageLoopNextIndex),
        (nextCompactionEligibleCount,sweepPageLoopNextPage)
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
    sweepNodeEdgesSizeofPtr <- gep (nullConst ppNodeType) [sweepNodeEdgesSize]
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
    condBr sweepIndexRangeCheck checkCompactEligible sweepPageLoop

    checkCompactEligible <- block
    compactEligibleCheck <- icmp uge (intConst 32 pageCompactionThreshold) sweepNextPageLiveCount
    condBr compactEligibleCheck isCompactEligible sweepPageLoopNextPage

    isCompactEligible <- block
    incrementedCompactionEligibleCount <- add compactionEligibleCount (intConst 32 1)
    br sweepPageLoopNextPage

    sweepPageLoopNextPage <- block
    nextCompactionEligibleCount <- phi [(compactionEligibleCount,checkCompactEligible),(incrementedCompactionEligibleCount,isCompactEligible)]
    sweepNextPagePtr <- gep sweepPage [intConst 32 0, intConst 32 0]
    sweepNextPage <- load sweepNextPagePtr 0
    sweepPageNullCheck <- icmp eq sweepNextPage (nullConst pPageType)
    condBr sweepPageNullCheck checkForNewPage sweepPageLoop

    checkForNewPage <- block
    compactionEligibleCountPtr <- gep globalState [intConst 32 0, intConst 32 2]
    store compactionEligibleCountPtr 0 nextCompactionEligibleCount
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
        pageType, -- root page
        i32 -- number of pages eligible for compaction
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
                    },
                Int { integerBits = 32, integerValue = 0 }
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

moveNodesName :: Name
moveNodesName = "moveNodes"

moveNodes :: Operand
moveNodes = functionRef moveNodesName [pFrameType,i32,ppNodeType] void

moveNodesImpl :: ModuleBuilder Operand
moveNodesImpl = function moveNodesName [(pFrameType,NoParameterName),(i32,NoParameterName),(ppNodeType,NoParameterName)] void $ \ [frame,nodeCount,nodeArray] -> mdo
    -- nodeCount is size of nodeArray
    -- every other element of nodeArray is the node to be moved
    -- interspersed with the node destination

    -- first, move the actual nodes in the pages
    entry <- block
    initialPage <- gep globalState [intConst 32 0, intConst 32 1]
    br moveActualNodesLoopLabel

    moveActualNodesLoopLabel <- block
    moveActualNodesIndex <- phi [(intConst 32 0,entry),(moveActualNodesNextIndex,moveActualNodesLoopBodyLabel)]
    moveActualNodesIndexP1 <- add moveActualNodesIndex (intConst 32 1)
    moveActualNodesNextIndex <- add moveActualNodesIndex (intConst 32 2)
    moveActualNodesIndexCheck <- icmp uge moveActualNodesIndex nodeCount
    condBr moveActualNodesIndexCheck pageLoopLabel moveActualNodesLoopBodyLabel

    moveActualNodesLoopBodyLabel <- block
    srcNodePtr <- gep nodeArray [moveActualNodesIndex]
    srcNode <- load srcNodePtr 0
    destNodePtr <- gep nodeArray [moveActualNodesIndexP1]
    destNode <- load destNodePtr 0
    call moveActualNode [srcNode,destNode]
    br moveActualNodesLoopLabel

    -- move the edges for all the nodes in all the pages
    pageLoopLabel <- block
    page <- phi [(initialPage,moveActualNodesLoopLabel),(nextPage,pageNodeLoopLabel)]
    pageCheck <- icmp eq page (nullConst pPageType)
    condBr pageCheck frameLoopLabel pageLoopBodyLabel

    pageLoopBodyLabel <- block
    nextPagePtr <- gep page [intConst 32 0,intConst 32 0]
    nextPage <- load nextPagePtr 0
    br pageNodeLoopLabel

    pageNodeLoopLabel <- block
    pageNodeIndex <- phi [(intConst 32 0,pageLoopBodyLabel),(pageNodeNextIndex,pageNodeLoopBodyLabel),(pageNodeNextIndex,pageNodeMoveEdgesLabel)]
    pageNodeNextIndex <- add pageNodeIndex (intConst 32 1)
    pageNodeIndexCheck <- icmp uge pageNodeIndex (intConst 32 pageSize)
    condBr pageNodeIndexCheck pageLoopLabel pageNodeLoopBodyLabel

    pageNodeLoopBodyLabel <- block
    pageNodeLivePtr <- gep page [intConst 32 0,intConst 32 1,pageNodeIndex,intConst 32 1]
    pageNodeLive <- load pageNodeLivePtr 0
    condBr pageNodeLive pageNodeMoveEdgesLabel pageNodeLoopLabel

    pageNodeMoveEdgesLabel <- block
    pageNodeEdgesArraySizePtr <- gep page [intConst 32 0,intConst 32 1,pageNodeIndex,intConst 32 2]
    pageNodeEdgesArraySize <- load pageNodeEdgesArraySizePtr 0
    pageNodeEdgesArrayPtr <- gep page [intConst 32 0,intConst 32 1,pageNodeIndex,intConst 32 3]
    pageNodeEdgesArray <- load pageNodeEdgesArrayPtr 0
    call moveNodeReferencesInEdgeArray [pageNodeEdgesArraySize,pageNodeEdgesArray,nodeCount,nodeArray]
    br pageNodeLoopLabel

    -- for each frame, move all the local variables, then all the edges
    -- in all the doEdgesIterators
    frameLoopLabel <- block
    currentFrame <- phi [(frame,pageLoopLabel),(nextFrame,doEdgesLoopLabel)]
    currentFrameCheck <- icmp eq currentFrame (nullConst pFrameType)
    condBr currentFrameCheck doneLabel frameLoopBodyLabel

    frameLoopBodyLabel <- block
    nextFramePtr <- gep currentFrame [intConst 32 0,intConst 32 0]
    nextFrame <- load nextFramePtr 0
    varArraySizePtr <- gep currentFrame [intConst 32 0,intConst 32 1]
    varArraySize <- load varArraySizePtr 0
    varArrayPtr <- gep currentFrame [intConst 32 0,intConst 32 2]
    varArray <- load varArrayPtr 0
    call moveNodeReferencesInEdgeArray [varArraySize,varArray,nodeCount,nodeArray]
    doEdgesArraySizePtr <- gep currentFrame [intConst 32 0,intConst 32 3]
    doEdgesArraySize <- load doEdgesArraySizePtr 0
    doEdgesArrayPtr <- gep currentFrame [intConst 32 0,intConst 32 4]
    doEdgesArray <- load doEdgesArrayPtr 0
    br doEdgesLoopLabel

    doEdgesLoopLabel <- block
    doEdgesIndex <- phi [(intConst 32 0,frameLoopBodyLabel),(doEdgesNextIndex,doEdgesLoopBodyLabel)]
    doEdgesNextIndex <- add doEdgesIndex (intConst 32 1)
    doEdgesIndexCheck <- icmp uge doEdgesIndex doEdgesArraySize
    condBr doEdgesIndexCheck frameLoopLabel doEdgesLoopBodyLabel

    doEdgesLoopBodyLabel <- block
    doEdgesEdgesArraySizePtr <- gep doEdgesArray [doEdgesIndex,intConst 32 0]
    doEdgesEdgesArraySize <- load doEdgesEdgesArraySizePtr 0
    doEdgesEdgesArrayPtr <- gep doEdgesArray [doEdgesIndex,intConst 32 1]
    doEdgesEdgesArray <- load doEdgesEdgesArrayPtr 0
    call moveNodeReferencesInEdgeArray [doEdgesEdgesArraySize,doEdgesEdgesArray,nodeCount,nodeArray]
    br doEdgesLoopLabel

    doneLabel <- block
    retVoid

moveActualNodeName :: Name
moveActualNodeName = "moveActualNode"

moveActualNode :: Operand
moveActualNode = functionRef moveActualNodeName [pNodeType,pNodeType] void

moveActualNodeImpl :: ModuleBuilder Operand
moveActualNodeImpl = function moveActualNodeName [(pNodeType,NoParameterName),(pNodeType,NoParameterName)] void $ \ [srcNode,destNode] -> mdo
    srcLivePtr <- gep srcNode [intConst 32 0,intConst 32 1]
    srcEdgeArraySizePtr <- gep srcNode [intConst 32 0,intConst 32 2]
    srcEdgeArrayPtr <- gep srcNode [intConst 32 0,intConst 32 3]
    destLivePtr <- gep destNode [intConst 32 0,intConst 32 1]
    destEdgeArraySizePtr <- gep destNode [intConst 32 0,intConst 32 2]
    destEdgeArrayPtr <- gep destNode [intConst 32 0,intConst 32 3]
    srcEdgeArraySize <- load srcEdgeArraySizePtr 0
    srcEdgeArray <- load srcEdgeArrayPtr 0
    destEdgeArraySize <- load destEdgeArraySizePtr 0
    destEdgeArray <- load destEdgeArrayPtr 0
    store srcLivePtr 0 (intConst 1 0)
    store srcEdgeArraySizePtr 0 destEdgeArraySize
    store srcEdgeArrayPtr 0 destEdgeArray
    store destLivePtr 0 (intConst 1 1)
    store destEdgeArraySizePtr 0 srcEdgeArraySize
    store destEdgeArrayPtr 0 srcEdgeArray
    retVoid

moveNodeReferenceName :: Name
moveNodeReferenceName = "moveNodeReference"

moveNodeReference :: Operand
moveNodeReference = functionRef moveNodeReferenceName [ppNodeType,i32,ppNodeType] void

moveNodeReferenceImpl :: ModuleBuilder Operand
moveNodeReferenceImpl = function moveNodeReferenceName [(ppNodeType,NoParameterName),(i32,NoParameterName),(ppNodeType,NoParameterName)] void $ \ [nodeReference,nodeCount,nodeArray] -> mdo
    -- nodeCount is size of nodeArray
    -- every other element of nodeArray is the node to be moved
    -- interspersed with the node destination
    entry <- block
    node <- load nodeReference 0
    nodeCheck <- icmp eq node (nullConst pNodeType)
    condBr nodeCheck doneLabel nodeArrayLoopLabel

    nodeArrayLoopLabel <- block
    index <- phi [(intConst 32 0,entry),(nextIndex,nodeArrayLoopBodyLabel)]
    nextIndex <- add index (intConst 32 2)
    indexCheck <- icmp uge index nodeCount
    condBr indexCheck doneLabel nodeArrayLoopBodyLabel

    nodeArrayLoopBodyLabel <- block
    srcNodePtr <- gep nodeArray [index]
    srcNode <- load srcNodePtr 0
    srcNodeCheck <- icmp eq srcNode node
    condBr srcNodeCheck moveNodeLabel nodeArrayLoopLabel

    moveNodeLabel <- block
    indexP1 <- add index (intConst 32 1)
    destNodePtr <- gep nodeArray [indexP1]
    destNode <- load destNodePtr 0
    store nodeReference 0 destNode
    br doneLabel

    doneLabel <- block
    retVoid

moveNodeReferencesInEdgeArrayName :: Name
moveNodeReferencesInEdgeArrayName = "moveNodeReferencesInEdgeArray"

moveNodeReferencesInEdgeArray :: Operand
moveNodeReferencesInEdgeArray = functionRef moveNodeReferencesInEdgeArrayName [i32,ppNodeType,i32,ppNodeType] void

moveNodeReferencesInEdgeArrayImpl :: ModuleBuilder Operand
moveNodeReferencesInEdgeArrayImpl = function moveNodeReferencesInEdgeArrayName [(i32,NoParameterName),(ppNodeType,NoParameterName),(i32,NoParameterName),(ppNodeType,NoParameterName)] void $ \ [edgeCount,edgeArray,nodeCount,nodeArray] -> mdo
    -- nodeCount is size of nodeArray
    -- every other element of nodeArray is the node to be moved
    -- interspersed with the node destination
    entry <- block
    br edgeLoopLabel

    edgeLoopLabel <- block
    index <- phi [(intConst 32 0,entry),(nextIndex,edgeLoopBodyLabel)]
    indexCheck <- icmp uge index edgeCount
    condBr indexCheck doneLabel edgeLoopBodyLabel

    edgeLoopBodyLabel <- block
    edgeReference <- gep edgeArray [index]
    call moveNodeReference [edgeReference,nodeCount,nodeArray]
    nextIndex <- add index (intConst 32 1)
    br edgeLoopLabel

    doneLabel <- block
    retVoid

freePageName :: Name
freePageName = "freePage"

freePage :: Operand
freePage = functionRef freePageName [pPageType,pPageType] void

freePageImpl :: ModuleBuilder Operand
freePageImpl = function freePageName [(pPageType,NoParameterName),(pPageType,NoParameterName)] void $ \ [page,previousPage] -> mdo
    entry <- block
    nextPagePtr <- gep page [intConst 32 0,intConst 32 0]
    nextPage <- load nextPagePtr 0
    previousNextPagePtr <- gep previousPage [intConst 32 0,intConst 32 0]
    store previousNextPagePtr 0 nextPage
    br nodeLoopLabel

    nodeLoopLabel <- block
    index <- phi [(intConst 32 0,entry),(nextIndex,nodeLoopBodyLabel),(nextIndex,freeEdgesLabel)]
    nextIndex <- add index (intConst 32 1)
    indexCheck <- icmp uge index (intConst 32 pageSize)
    condBr indexCheck doneLabel nodeLoopBodyLabel

    nodeLoopBodyLabel <- block
    edgesArrayPtr <- gep page [intConst 32 0,intConst 32 1,index,intConst 32 3]
    edgesArray <- load edgesArrayPtr 0
    edgesArrayCheck <- icmp eq edgesArray (nullConst ppNodeType)
    condBr edgesArrayCheck nodeLoopLabel freeEdgesLabel

    freeEdgesLabel <- block
    edgesArrayRawPtr <- bitcast edgesArray (ptr i8)
    call free [edgesArrayRawPtr]
    br nodeLoopLabel

    doneLabel <- block
    pageRawPtr <- bitcast page (ptr i8)
    call free [pageRawPtr]
    retVoid

compactName :: Name
compactName = "compact"

compact :: Operand
compact = functionRef compactName [pFrameType] void

compactDecl :: ModuleBuilder Operand
compactDecl = extern compactName [pFrameType] void

compactImpl :: ModuleBuilder Operand
compactImpl = function compactName [(pFrameType,NoParameterName)] void $ \ [frame] -> mdo
    compactionEligibleCountPtr <- gep globalState [intConst 32 0, intConst 32 2]
    compactionEligibleCount <- load compactionEligibleCountPtr 0
    compactionEligibleCountCheck <- icmp uge compactionEligibleCount (intConst 32 2)
    condBr compactionEligibleCountCheck startCompactionCheckLabel doneLabel

    startCompactionCheckLabel <- block
    initialPage <- gep globalState [intConst 32 0,intConst 32 1]
    moveNodeArraySize <- add (intConst 32 pageCompactionThreshold) (intConst 32 pageCompactionThreshold)
    moveNodeArray <- alloca pNodeType (Just moveNodeArraySize) 0
    store compactionEligibleCountPtr 0 (intConst 32 0)
    br compactionCheckLoopLabel

    compactionCheckLoopLabel <- block
    page <- phi [
        (initialPage,startCompactionCheckLabel),
        (nextPage,checkPageLabel),
        (nextPage,pageEligibleLabel),
        (initialPage,computeNewEligibleCountLabel)
        ]
    previousPage <- phi [
        (nullConst pPageType,startCompactionCheckLabel),
        (page,checkPageLabel),
        (page,pageEligibleLabel),
        (nullConst pPageType,computeNewEligibleCountLabel)
        ]
    targetPage <- phi [
        (nullConst pPageType,startCompactionCheckLabel),
        (targetPage,checkPageLabel),
        (srcPage,pageEligibleLabel),
        (nullConst pPageType,computeNewEligibleCountLabel)
        ]
    targetLiveCount <- phi [
        (intConst 32 0,startCompactionCheckLabel),
        (targetLiveCount,checkPageLabel),
        (srcLiveCount,pageEligibleLabel),
        (intConst 32 0,computeNewEligibleCountLabel)
        ]
    srcPreviousPage <- phi [
        (nullConst pPageType,startCompactionCheckLabel),
        (srcPreviousPage,checkPageLabel),
        (previousPage,pageEligibleLabel),
        (nullConst pPageType,computeNewEligibleCountLabel)
        ]
    srcPage <- phi [
        (nullConst pPageType,startCompactionCheckLabel),
        (srcPage,checkPageLabel),
        (page,pageEligibleLabel),
        (nullConst pPageType,computeNewEligibleCountLabel)
        ]
    srcLiveCount <- phi [
        (intConst 32 0,startCompactionCheckLabel),
        (srcLiveCount,checkPageLabel),
        (liveCount,pageEligibleLabel),
        (intConst 32 0,computeNewEligibleCountLabel)
        ]
    stillEligibleCount <- phi [
        (intConst 32 0,startCompactionCheckLabel),
        (stillEligibleCount,checkPageLabel),
        (incrementedStillEligibleCount,pageEligibleLabel),
        (intConst 32 0,computeNewEligibleCountLabel)
        ]
    pageCheck <- icmp eq page (nullConst pPageType)
    condBr pageCheck moveNodesCheckLabel countLiveLoopLabel

    countLiveLoopLabel <- block
    index <- phi [
        (intConst 32 0,compactionCheckLoopLabel),
        (nextIndex,checkNodeLabel),
        (nextIndex,incrementLiveCountLabel)
        ]
    liveCount <- phi [
        (intConst 32 0,compactionCheckLoopLabel),
        (liveCount,checkNodeLabel),
        (incrementedLiveCount,incrementLiveCountLabel)
        ]
    indexCheck <- icmp uge index (intConst 32 pageSize)
    condBr indexCheck checkPageLabel checkNodeLabel

    checkNodeLabel <- block
    nextIndex <- add index (intConst 32 1)
    livePtr <- gep page [intConst 32 0,intConst 32 1,index,intConst 32 1]
    live <- load livePtr 0
    condBr live incrementLiveCountLabel countLiveLoopLabel

    incrementLiveCountLabel <- block
    incrementedLiveCount <- add liveCount (intConst 32 1)
    br countLiveLoopLabel

    checkPageLabel <- block
    nextPagePtr <- gep page [intConst 32 0,intConst 32 0]
    nextPage <- load nextPagePtr 0
    liveCountCheck <- icmp uge liveCount (intConst 32 pageCompactionThreshold)
    condBr liveCountCheck compactionCheckLoopLabel pageEligibleLabel

    pageEligibleLabel <- block
    incrementedStillEligibleCount <- add stillEligibleCount (intConst 32 1)
    br compactionCheckLoopLabel
    
    moveNodesCheckLabel <- block
    targetPageCheck <- icmp eq targetPage (nullConst pPageType)
    condBr targetPageCheck doneLabel initMoveNodesLabel

    initMoveNodesLabel <- block
    moveNodeCount <- add srcLiveCount srcLiveCount
    br collectMoveNodesLoopLabel

    collectMoveNodesLoopLabel <- block
    moveNodeIndex <- phi [
        (intConst 32 0,initMoveNodesLabel),
        (moveNodeNextIndex,collectMoveNodeLabel)
        ]
    srcPageIndex0 <- phi [
        (intConst 32 0,initMoveNodesLabel),
        (srcPageNextIndex1,collectMoveNodeLabel)
        ]
    targetPageIndex0 <- phi [
        (intConst 32 0,initMoveNodesLabel),
        (targetPageNextIndex1,collectMoveNodeLabel)
        ]
    moveNodeIndexCheck <- icmp uge moveNodeIndex moveNodeCount
    condBr moveNodeIndexCheck callMoveNodesLabel findLiveSrcPageIndexLabel

    findLiveSrcPageIndexLabel <- block
    srcPageIndex1 <- phi [
        (srcPageIndex0,collectMoveNodesLoopLabel),
        (srcPageNextIndex1,findLiveSrcPageIndexLabel)
        ]
    srcPageNextIndex1 <- add srcPageIndex1 (intConst 32 1)
    srcPageNodeLivePtr <- gep srcPage [intConst 32 0,intConst 32 1,srcPageIndex1,intConst 32 1]
    srcPageNodeLive <- load srcPageNodeLivePtr 0
    condBr srcPageNodeLive findFreeTargetPageIndexLabel findLiveSrcPageIndexLabel

    findFreeTargetPageIndexLabel <- block
    targetPageIndex1 <- phi [
        (targetPageIndex0,findLiveSrcPageIndexLabel),
        (targetPageNextIndex1,findFreeTargetPageIndexLabel)
        ]
    targetPageNextIndex1 <- add targetPageIndex1 (intConst 32 1)
    targetPageNodeLivePtr <- gep targetPage [intConst 32 0,intConst 32 1,targetPageIndex1,intConst 32 1]
    targetPageNodeLive <- load targetPageNodeLivePtr 0
    condBr targetPageNodeLive findFreeTargetPageIndexLabel collectMoveNodeLabel

    collectMoveNodeLabel <- block
    srcPageNode <- gep srcPage [intConst 32 0,intConst 32 1,srcPageIndex1]
    moveNodeArraySrcPtr <- gep moveNodeArray [moveNodeIndex]
    store moveNodeArraySrcPtr 0 srcPageNode
    targetPageNode <- gep targetPage [intConst 32 0,intConst 32 1,targetPageIndex1]
    moveNodeIndexP1 <- add moveNodeIndex (intConst 32 1)
    moveNodeArrayTargetPtr <- gep moveNodeArray [moveNodeIndexP1]
    store moveNodeArrayTargetPtr 0 targetPageNode
    moveNodeNextIndex <- add moveNodeIndex (intConst 32 2)
    br collectMoveNodesLoopLabel

    callMoveNodesLabel <- block
    call moveNodes [frame,moveNodeCount,moveNodeArray]
    call freePage [srcPage,srcPreviousPage]
    newTargetLiveCount <- add targetLiveCount srcLiveCount
    newTargetLiveCountCheck <- icmp uge (intConst 32 pageCompactionThreshold) newTargetLiveCount
    condBr newTargetLiveCountCheck targetStillEligibleLabel computeNewEligibleCountLabel

    targetStillEligibleLabel <- block
    br computeNewEligibleCountLabel

    computeNewEligibleCountLabel <- block
    noLongerEligibleCount <- phi [(intConst 32 1,targetStillEligibleLabel),(intConst 32 2,callMoveNodesLabel)]
    newStillEligibleCount <- sub stillEligibleCount noLongerEligibleCount
    newStillEligibleCountCheck <- icmp uge newStillEligibleCount (intConst 32 2)
    condBr newStillEligibleCountCheck compactionCheckLoopLabel doneLabel

    doneLabel <- block
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
    moveNodesImpl
    moveActualNodeImpl
    moveNodeReferenceImpl
    moveNodeReferencesInEdgeArrayImpl
    freePageImpl
    compactImpl
    traceDefs
    return ()

runtimeDecls :: ModuleBuilder ()
runtimeDecls = do
    commonDecls
    newNodeDecl
    hasEdgeDecl
    addEdgeDecl
    removeEdgeDecl
    compactDecl
    traceDefs
    return ()

traceEnabled :: Bool
traceEnabled = False

traceFmtTable :: GlobalStrzTable
traceFmtTable = GlobalStrzTable "traceFmt" [
    "%d: LET=\n",
    "%d: LET>\n",
    "%d: LET<\n",
    "%d: CALL\n",
    "%d: RETURN\n",
    "%d: DO\n",
    "%d: DO<\n",
    "%d: EXIT\n",
    "%d: IF=\n",
    "%d: IF>\n",
    "%d: ELSE\n",
    "%d: LABEL %d\n"
    ]

traceDefs :: ModuleBuilder ()
traceDefs
 | traceEnabled = do
        globalStrzDefs traceFmtTable
        varArgsExtern (fromString "printf") [ptr i8] void
        extern (fromString "fflush") [ptr i8] void
        return ()
 | otherwise = return ()

trace :: (MonadIRBuilder m, MonadFix m) => Integer -> String -> m ()
trace lineNumber fmt = tracef fmt [intConst 32 lineNumber]

traceLabel :: (MonadIRBuilder m, MonadFix m) => Integer -> Name -> String -> m ()
traceLabel lineNumber (UnName n) fmt = tracef fmt [intConst 32 lineNumber,intConst 32 $ fromIntegral n]
traceLabel lineNumber _ fmt = return ()

tracef :: (MonadIRBuilder m, MonadFix m) => String -> [Operand] -> m ()
tracef fmt args
 | traceEnabled = do
        fmt <- globalStrzAsI8Ptr traceFmtTable fmt
        call (varArgsFunctionRef (fromString "printf") [ptr i8] void) (fmt:args)
        call (functionRef (fromString "fflush") [ptr i8] void) [nullConst (ptr i8)]
        return ()
 | otherwise = return ()
