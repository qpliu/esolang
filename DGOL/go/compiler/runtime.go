package main

import (
	"llvm.org/llvm/bindings/go/llvm"
)

const (
	pageSize                = 256
	newPageThreshold        = 240
	pageCompactionThreshold = 16

	edgeArrayIncrement = 16
)

type RTDecls struct {
	NodeType             llvm.Type
	PNodeType            llvm.Type
	PPNodeType           llvm.Type
	PPPNodeType          llvm.Type
	DoEdgesIteratorType  llvm.Type
	PDoEdgesIteratorType llvm.Type
	FrameType            llvm.Type
	PFrameType           llvm.Type

	Memset llvm.Value
	Memcpy llvm.Value
	Malloc llvm.Value
	Free   llvm.Value

	NewNode    llvm.Value
	HasEdge    llvm.Value
	AddEdge    llvm.Value
	RemoveEdge llvm.Value
	Compact    llvm.Value

	PageType    llvm.Type
	PPageType   llvm.Type
	GlobalState llvm.Value
}

const (
	newNodeName    = "newNode"
	hasEdgeName    = "hasEdge"
	addEdgeName    = "addEdge"
	removeEdgeName = "removeEdge"
	compactName    = "compact"
)

func RuntimeDecls(context llvm.Context, name string) (llvm.Module, *RTDecls) {
	mod := context.NewModule(name)
	decls := &RTDecls{}
	decls.NodeType = mod.Context().StructCreateNamed("node")
	decls.PNodeType = llvm.PointerType(decls.NodeType, 0)
	decls.PPNodeType = llvm.PointerType(decls.PNodeType, 0)
	decls.PPPNodeType = llvm.PointerType(decls.PPNodeType, 0)
	decls.NodeType.StructSetBody([]llvm.Type{
		llvm.Int8Type(),  // gc mark
		llvm.Int1Type(),  // live flag
		llvm.Int32Type(), // size of allocated edges array
		decls.PPNodeType, // array of edges
	}, false)

	decls.DoEdgesIteratorType = mod.Context().StructCreateNamed("doEdgesIterator")
	decls.PDoEdgesIteratorType = llvm.PointerType(decls.DoEdgesIteratorType, 0)
	decls.DoEdgesIteratorType.StructSetBody([]llvm.Type{
		llvm.Int32Type(), // size of allocated edges array
		decls.PPNodeType, // array of edges
	}, false)

	decls.FrameType = mod.Context().StructCreateNamed("frame")
	decls.PFrameType = llvm.PointerType(decls.FrameType, 0)
	decls.FrameType.StructSetBody([]llvm.Type{
		decls.PFrameType,           // caller frame
		llvm.Int32Type(),           // number of local vars
		decls.PPNodeType,           // local vars (stack allocated)
		llvm.Int32Type(),           // number of doEdgesIterators
		decls.PDoEdgesIteratorType, // doEdgesIterators (stack allocated)
		llvm.Int32Type(),           // number of call args
		decls.PPPNodeType,          // call args (stack allocated)
	}, false)

	decls.Memset = llvm.AddFunction(mod, "llvm.memset.p0i8.i32", llvm.FunctionType(llvm.VoidType(), []llvm.Type{
		llvm.PointerType(llvm.Int8Type(), 0),
		llvm.Int8Type(),
		llvm.Int32Type(),
		llvm.Int32Type(),
		llvm.Int1Type(),
	}, false))
	decls.Memcpy = llvm.AddFunction(mod, "llvm.memcpy.p0i8.p0i8.i32", llvm.FunctionType(llvm.VoidType(), []llvm.Type{
		llvm.PointerType(llvm.Int8Type(), 0),
		llvm.PointerType(llvm.Int8Type(), 0),
		llvm.Int32Type(),
		llvm.Int32Type(),
		llvm.Int1Type(),
	}, false))
	decls.Malloc = llvm.AddFunction(mod, "malloc", llvm.FunctionType(llvm.PointerType(llvm.Int8Type(), 0), []llvm.Type{
		llvm.Int32Type(),
	}, false))
	decls.Free = llvm.AddFunction(mod, "free", llvm.FunctionType(llvm.VoidType(), []llvm.Type{
		llvm.PointerType(llvm.Int8Type(), 0),
	}, false))

	decls.NewNode = llvm.AddFunction(mod, newNodeName, llvm.FunctionType(decls.PNodeType, []llvm.Type{decls.PFrameType}, false))
	decls.HasEdge = llvm.AddFunction(mod, hasEdgeName, llvm.FunctionType(llvm.Int1Type(), []llvm.Type{decls.PNodeType, decls.PNodeType}, false))
	decls.AddEdge = llvm.AddFunction(mod, addEdgeName, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PNodeType, decls.PNodeType}, false))
	decls.RemoveEdge = llvm.AddFunction(mod, removeEdgeName, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PNodeType, decls.PNodeType}, false))
	decls.Compact = llvm.AddFunction(mod, compactName, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))

	return mod, decls
}

func RuntimeDefs(context llvm.Context, name string) (llvm.Module, *RTDecls) {
	mod, decls := RuntimeDecls(context, name)

	decls.PageType = mod.Context().StructCreateNamed("page")
	decls.PPageType = llvm.PointerType(decls.PageType, 0)
	decls.PageType.StructSetBody([]llvm.Type{
		decls.PPageType, // link to next page
		llvm.ArrayType(decls.NodeType, pageSize),
	}, false)

	decls.GlobalState = llvm.AddGlobal(mod, llvm.StructType([]llvm.Type{
		llvm.Int8Type(),  // gc mark
		decls.PageType,   // root page
		llvm.Int32Type(), // number of pages eligible for compaction
	}, false), "globalState")
	emptyNodes := make([]llvm.Value, pageSize)
	for i := range emptyNodes {
		emptyNodes[i] = llvm.ConstNamedStruct(decls.NodeType, []llvm.Value{
			llvm.ConstInt(llvm.Int8Type(), 0, false),
			llvm.ConstInt(llvm.Int1Type(), 0, false),
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstNull(decls.PPNodeType),
		})
	}
	decls.GlobalState.SetInitializer(llvm.ConstStruct([]llvm.Value{
		llvm.ConstInt(llvm.Int8Type(), 0, false),
		llvm.ConstNamedStruct(decls.PageType, []llvm.Value{
			llvm.ConstNull(decls.PPageType),
			llvm.ConstArray(decls.NodeType, emptyNodes),
		}),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, false))

	defNewNode(mod, decls)
	defHasEdge(mod, decls)
	defAddEdge(mod, decls)
	defRemoveEdge(mod, decls)
	defCompact(mod, decls)
	return mod, decls
}

func defNewNode(mod llvm.Module, decls *RTDecls) {
	b := mod.Context().NewBuilder()
	defer b.Dispose()
	entryBlock := llvm.AddBasicBlock(decls.NewNode, "")

	frameParam := decls.NewNode.FirstParam()

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	retryNewNodeBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(retryNewNodeBlock)

	b.SetInsertPoint(retryNewNodeBlock, retryNewNodeBlock.FirstInstruction())
	initialPage := llvm.ConstGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	})

	newNodePageLoopBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(newNodePageLoopBlock)

	// iterate over nodes ands pages
	b.SetInsertPoint(newNodePageLoopBlock, newNodePageLoopBlock.FirstInstruction())
	newNodePage := b.CreatePHI(decls.PPageType, "")
	var newNodeNextPage llvm.Value
	var newNodePageLoopNextIndexBlock, newNodePageLoopNextPageBlock llvm.BasicBlock
	defer func() {
		newNodePage.AddIncoming([]llvm.Value{
			initialPage,
			newNodePage,
			newNodeNextPage,
		}, []llvm.BasicBlock{
			retryNewNodeBlock,
			newNodePageLoopNextIndexBlock,
			newNodePageLoopNextPageBlock,
		})
	}()
	newNodeIndex := b.CreatePHI(llvm.Int32Type(), "")
	var newNodeNextIndex llvm.Value
	defer func() {
		newNodeIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			newNodeNextIndex,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
		}, []llvm.BasicBlock{
			retryNewNodeBlock,
			newNodePageLoopNextIndexBlock,
			newNodePageLoopNextPageBlock,
		})
	}()
	newNodeAlivePtr := b.CreateGEP(newNodePage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		newNodeIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	newNodeAlive := b.CreateLoad(newNodeAlivePtr, "")
	returnNewNodeBlock := llvm.AddBasicBlock(decls.NewNode, "")
	newNodePageLoopNextIndexBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(newNodeAlive, newNodePageLoopNextIndexBlock, returnNewNodeBlock)

	// found free node
	b.SetInsertPoint(returnNewNodeBlock, returnNewNodeBlock.FirstInstruction())
	b.CreateStore(llvm.ConstInt(llvm.Int1Type(), 1, false), newNodeAlivePtr)
	newNodePtr := b.CreateGEP(newNodePage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		newNodeIndex,
	}, "")
	b.CreateRet(newNodePtr)

	// iterate to next node in page
	b.SetInsertPoint(newNodePageLoopNextIndexBlock, newNodePageLoopNextIndexBlock.FirstInstruction())
	newNodeNextIndex = b.CreateAdd(newNodeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	newNodeNextIndexCheck := b.CreateICmp(llvm.IntUGE, newNodeNextIndex, llvm.ConstInt(llvm.Int32Type(), pageSize, false), "")
	newNodePageLoopNextPageBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(newNodeNextIndexCheck, newNodePageLoopNextPageBlock, newNodePageLoopBlock)

	// iterate to next page
	b.SetInsertPoint(newNodePageLoopNextPageBlock, newNodePageLoopNextPageBlock.FirstInstruction())
	newNodeNextPagePtr := b.CreateGEP(newNodePage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	newNodeNextPage = b.CreateLoad(newNodeNextPagePtr, "")
	newNodeNextPageCheck := b.CreateICmp(llvm.IntEQ, newNodeNextPage, llvm.ConstNull(decls.PPageType), "")
	startGCMarkBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(newNodeNextPageCheck, startGCMarkBlock, newNodePageLoopBlock)

	// mark phase
	b.SetInsertPoint(startGCMarkBlock, startGCMarkBlock.FirstInstruction())
	gcMarkPtr := b.CreateGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	oldGcMark := b.CreateLoad(gcMarkPtr, "")
	newGcMark := b.CreateAdd(oldGcMark, llvm.ConstInt(llvm.Int8Type(), 1, false), "")
	b.CreateStore(newGcMark, gcMarkPtr)
	markFrameLoopBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(markFrameLoopBlock)

	// mark phase: iterate over frames
	b.SetInsertPoint(markFrameLoopBlock, markFrameLoopBlock.FirstInstruction())
	frame := b.CreatePHI(decls.PFrameType, "")
	var markNextFrame llvm.Value
	var markEdgesDoIteratorsLoopBlock llvm.BasicBlock
	defer func() {
		frame.AddIncoming([]llvm.Value{
			frameParam,
			markNextFrame,
		}, []llvm.BasicBlock{
			startGCMarkBlock,
			markEdgesDoIteratorsLoopBlock,
		})
	}()
	frameCheck := b.CreateICmp(llvm.IntEQ, frame, llvm.ConstNull(decls.PFrameType), "")
	markFrameLoopBodyBlock := llvm.AddBasicBlock(decls.NewNode, "")
	startGCSweepBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(frameCheck, startGCSweepBlock, markFrameLoopBodyBlock)

	b.SetInsertPoint(markFrameLoopBodyBlock, markFrameLoopBodyBlock.FirstInstruction())
	markNextFramePtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	markNextFrame = b.CreateLoad(markNextFramePtr, "")
	markVarArraySizePtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	markVarArraySize := b.CreateLoad(markVarArraySizePtr, "")
	markVarArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	markVarArray := b.CreateLoad(markVarArrayPtr, "")
	markDoEdgesIteratorSizePtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	markDoEdgesIteratorSize := b.CreateLoad(markDoEdgesIteratorSizePtr, "")
	markDoEdgesIteratorArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 4, false),
	}, "")
	markDoEdgesIteratorArray := b.CreateLoad(markDoEdgesIteratorArrayPtr, "")
	markVarLoopBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(markVarLoopBlock)

	// iterate over vars in frame
	b.SetInsertPoint(markVarLoopBlock, markVarLoopBlock.FirstInstruction())
	//...
	b.CreateRet(llvm.ConstNull(decls.PNodeType))

	markEdgesDoIteratorsLoopBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.SetInsertPoint(markEdgesDoIteratorsLoopBlock, markEdgesDoIteratorsLoopBlock.FirstInstruction())
	//...
	markNextFrame = frame
	b.CreateBr(markFrameLoopBlock)

	_, _, _, _ = markVarArraySize, markVarArray, markDoEdgesIteratorSize, markDoEdgesIteratorArray

	b.SetInsertPoint(startGCSweepBlock, startGCSweepBlock.FirstInstruction())
	//...
	b.CreateRet(llvm.ConstNull(decls.PNodeType))
}

func defHasEdge(mod llvm.Module, decls *RTDecls) {
	b := mod.Context().NewBuilder()
	defer b.Dispose()

	nodeParam := decls.HasEdge.FirstParam()
	edgeParam := llvm.NextParam(nodeParam)

	entryBlock := llvm.AddBasicBlock(decls.HasEdge, "")
	edgeLoopBlock := llvm.AddBasicBlock(decls.HasEdge, "")
	checkCurrentEdgeBlock := llvm.AddBasicBlock(decls.HasEdge, "")
	retTrueBlock := llvm.AddBasicBlock(decls.HasEdge, "")
	retFalseBlock := llvm.AddBasicBlock(decls.HasEdge, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	edgeArraySizePtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	edgeArraySize := b.CreateLoad(edgeArraySizePtr, "")
	edgeArrayPtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	edgeArray := b.CreateLoad(edgeArrayPtr, "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(edgeLoopBlock, edgeLoopBlock.FirstInstruction())
	edgeIndex := b.CreatePHI(llvm.Int32Type(), "")
	edgeNextIndex := b.CreateAdd(edgeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	edgeIndex.AddIncoming([]llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		edgeNextIndex,
	}, []llvm.BasicBlock{
		entryBlock,
		checkCurrentEdgeBlock,
	})
	edgeIndexCheck := b.CreateICmp(llvm.IntUGE, edgeIndex, edgeArraySize, "")
	b.CreateCondBr(edgeIndexCheck, retFalseBlock, checkCurrentEdgeBlock)

	b.SetInsertPoint(checkCurrentEdgeBlock, checkCurrentEdgeBlock.FirstInstruction())
	currentEdgePtr := b.CreateGEP(edgeArray, []llvm.Value{edgeIndex}, "")
	currentEdge := b.CreateLoad(currentEdgePtr, "")
	currentEdgeCheck := b.CreateICmp(llvm.IntEQ, currentEdge, edgeParam, "")
	b.CreateCondBr(currentEdgeCheck, retTrueBlock, edgeLoopBlock)

	b.SetInsertPoint(retTrueBlock, retTrueBlock.FirstInstruction())
	b.CreateRet(llvm.ConstInt(llvm.Int1Type(), 1, false))

	b.SetInsertPoint(retFalseBlock, retFalseBlock.FirstInstruction())
	b.CreateRet(llvm.ConstInt(llvm.Int1Type(), 0, false))
}

func defAddEdge(mod llvm.Module, decls *RTDecls) {
	b := mod.Context().NewBuilder()
	defer b.Dispose()

	nodeParam := decls.AddEdge.FirstParam()
	edgeParam := llvm.NextParam(nodeParam)

	entryBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	initLoopBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	loopBodyBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	checkCurrentEdgeBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	foundFreeSlotBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	reallocEdgeArrayBlock := llvm.AddBasicBlock(decls.AddEdge, "")
	doneBlock := llvm.AddBasicBlock(decls.AddEdge, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	alreadyHasEdgeCheck := b.CreateCall(decls.HasEdge, []llvm.Value{nodeParam, edgeParam}, "")
	b.CreateCondBr(alreadyHasEdgeCheck, doneBlock, initLoopBlock)

	b.SetInsertPoint(initLoopBlock, initLoopBlock.FirstInstruction())
	edgeArraySizePtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	initEdgeArraySize := b.CreateLoad(edgeArraySizePtr, "")
	edgeArrayPtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	initEdgeArray := b.CreateLoad(edgeArrayPtr, "")
	b.CreateBr(loopBodyBlock)

	b.SetInsertPoint(loopBodyBlock, loopBodyBlock.FirstInstruction())
	edgeArraySize := b.CreatePHI(llvm.Int32Type(), "")
	var newEdgeArraySize llvm.Value
	defer func() {
		edgeArraySize.AddIncoming([]llvm.Value{
			initEdgeArraySize,
			edgeArraySize,
			newEdgeArraySize,
		}, []llvm.BasicBlock{
			initLoopBlock,
			checkCurrentEdgeBlock,
			reallocEdgeArrayBlock,
		})
	}()
	edgeArray := b.CreatePHI(decls.PPNodeType, "")
	var newEdgeArray llvm.Value
	defer func() {
		edgeArray.AddIncoming([]llvm.Value{
			initEdgeArray,
			edgeArray,
			newEdgeArray,
		}, []llvm.BasicBlock{
			initLoopBlock,
			checkCurrentEdgeBlock,
			reallocEdgeArrayBlock,
		})
	}()
	edgeIndex := b.CreatePHI(llvm.Int32Type(), "")
	var edgeNextIndex llvm.Value
	defer func() {
		edgeIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			edgeNextIndex,
			edgeArraySize,
		}, []llvm.BasicBlock{
			initLoopBlock,
			checkCurrentEdgeBlock,
			reallocEdgeArrayBlock,
		})
	}()
	edgeNextIndex = b.CreateAdd(edgeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	edgeIndexCheck := b.CreateICmp(llvm.IntUGE, edgeIndex, edgeArraySize, "")
	b.CreateCondBr(edgeIndexCheck, reallocEdgeArrayBlock, checkCurrentEdgeBlock)

	b.SetInsertPoint(checkCurrentEdgeBlock, checkCurrentEdgeBlock.FirstInstruction())
	currentEdgePtr := b.CreateGEP(edgeArray, []llvm.Value{edgeIndex}, "")
	currentEdge := b.CreateLoad(currentEdgePtr, "")
	currentEdgeCheck := b.CreateICmp(llvm.IntEQ, currentEdge, llvm.ConstNull(decls.PNodeType), "")
	b.CreateCondBr(currentEdgeCheck, foundFreeSlotBlock, loopBodyBlock)

	b.SetInsertPoint(foundFreeSlotBlock, foundFreeSlotBlock.FirstInstruction())
	b.CreateStore(edgeParam, currentEdgePtr)
	b.CreateBr(doneBlock)

	b.SetInsertPoint(reallocEdgeArrayBlock, reallocEdgeArrayBlock.FirstInstruction())
	newEdgeArraySize = b.CreateAdd(edgeArraySize, llvm.ConstInt(llvm.Int32Type(), edgeArrayIncrement, false), "")
	newEdgeArrayAllocSizePtr := b.CreateGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{newEdgeArraySize}, "")
	newEdgeArrayAllocSize := b.CreatePtrToInt(newEdgeArrayAllocSizePtr, llvm.Int32Type(), "")
	newEdgeArrayRawPtr := b.CreateCall(decls.Malloc, []llvm.Value{newEdgeArrayAllocSize}, "")
	oldEdgeArrayRawPtr := b.CreateBitCast(edgeArray, llvm.PointerType(llvm.Int8Type(), 0), "")
	oldEdgeArrayAllocSizePtr := b.CreateGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{edgeArraySize}, "")
	oldEdgeArrayAllocSize := b.CreatePtrToInt(oldEdgeArrayAllocSizePtr, llvm.Int32Type(), "")
	b.CreateCall(decls.Memcpy, []llvm.Value{
		newEdgeArrayRawPtr,
		oldEdgeArrayRawPtr,
		oldEdgeArrayAllocSize,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int1Type(), 0, false),
	}, "")
	edgeArrayIncrementSizePtr := b.CreateGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), edgeArrayIncrement, false)}, "")
	edgeArrayIncrementSize := b.CreatePtrToInt(edgeArrayIncrementSizePtr, llvm.Int32Type(), "")
	edgeArrayIncrementArray := b.CreateGEP(newEdgeArrayRawPtr, []llvm.Value{oldEdgeArrayAllocSize}, "")
	b.CreateCall(decls.Memset, []llvm.Value{
		edgeArrayIncrementArray,
		llvm.ConstInt(llvm.Int8Type(), 0, false),
		edgeArrayIncrementSize,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int1Type(), 0, false),
	}, "")
	b.CreateStore(newEdgeArraySize, edgeArraySizePtr)
	newEdgeArray = b.CreateBitCast(newEdgeArrayRawPtr, decls.PPNodeType, "")
	b.CreateStore(newEdgeArray, edgeArrayPtr)
	b.CreateBr(loopBodyBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()
}

func defRemoveEdge(mod llvm.Module, decls *RTDecls) {
	b := mod.Context().NewBuilder()
	defer b.Dispose()

	nodeParam := decls.RemoveEdge.FirstParam()
	edgeParam := llvm.NextParam(nodeParam)

	entryBlock := llvm.AddBasicBlock(decls.RemoveEdge, "")
	edgeLoopBlock := llvm.AddBasicBlock(decls.RemoveEdge, "")
	checkCurrentEdgeBlock := llvm.AddBasicBlock(decls.RemoveEdge, "")
	removeBlock := llvm.AddBasicBlock(decls.RemoveEdge, "")
	doneBlock := llvm.AddBasicBlock(decls.RemoveEdge, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	edgeArraySizePtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	edgeArraySize := b.CreateLoad(edgeArraySizePtr, "")
	edgeArrayPtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	edgeArray := b.CreateLoad(edgeArrayPtr, "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(edgeLoopBlock, edgeLoopBlock.FirstInstruction())
	edgeIndex := b.CreatePHI(llvm.Int32Type(), "")
	edgeNextIndex := b.CreateAdd(edgeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	edgeIndex.AddIncoming([]llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		edgeNextIndex,
	}, []llvm.BasicBlock{
		entryBlock,
		checkCurrentEdgeBlock,
	})
	edgeIndexCheck := b.CreateICmp(llvm.IntUGE, edgeIndex, edgeArraySize, "")
	b.CreateCondBr(edgeIndexCheck, doneBlock, checkCurrentEdgeBlock)

	b.SetInsertPoint(checkCurrentEdgeBlock, checkCurrentEdgeBlock.FirstInstruction())
	currentEdgePtr := b.CreateGEP(edgeArray, []llvm.Value{edgeIndex}, "")
	currentEdge := b.CreateLoad(currentEdgePtr, "")
	currentEdgeCheck := b.CreateICmp(llvm.IntEQ, currentEdge, edgeParam, "")
	b.CreateCondBr(currentEdgeCheck, removeBlock, edgeLoopBlock)

	b.SetInsertPoint(removeBlock, removeBlock.FirstInstruction())
	b.CreateStore(llvm.ConstNull(decls.PNodeType), currentEdgePtr)
	b.CreateBr(doneBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()
}

func defCompact(mod llvm.Module, decls *RTDecls) {
	b := mod.Context().NewBuilder()
	defer b.Dispose()
	entryBlock := llvm.AddBasicBlock(decls.Compact, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	//...
	b.CreateRetVoid()
}
