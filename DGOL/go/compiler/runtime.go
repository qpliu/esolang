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
	markNodes := defMarkNodes(mod, decls)

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
	var markDoEdgesIteratorsLoopBlock llvm.BasicBlock
	defer func() {
		frame.AddIncoming([]llvm.Value{
			frameParam,
			markNextFrame,
		}, []llvm.BasicBlock{
			startGCMarkBlock,
			markDoEdgesIteratorsLoopBlock,
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
	markDoEdgesIteratorsSizePtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	markDoEdgesIteratorsSize := b.CreateLoad(markDoEdgesIteratorsSizePtr, "")
	markDoEdgesIteratorsArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 4, false),
	}, "")
	markDoEdgesIteratorsArray := b.CreateLoad(markDoEdgesIteratorsArrayPtr, "")
	markVarLoopBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(markVarLoopBlock)

	// mark phase: iterate over vars in frame
	b.SetInsertPoint(markVarLoopBlock, markVarLoopBlock.FirstInstruction())
	markVarIndex := b.CreatePHI(llvm.Int32Type(), "")
	var markVarNextIndex llvm.Value
	var markVarLoopBodyBlock llvm.BasicBlock
	defer func() {
		markVarIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			markVarNextIndex,
		}, []llvm.BasicBlock{
			markFrameLoopBodyBlock,
			markVarLoopBodyBlock,
		})
	}()
	markVarNextIndex = b.CreateAdd(markVarIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	markVarIndexCheck := b.CreateICmp(llvm.IntUGE, markVarIndex, markVarArraySize, "")
	markVarLoopBodyBlock = llvm.AddBasicBlock(decls.NewNode, "")
	markDoEdgesIteratorsLoopBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(markVarIndexCheck, markDoEdgesIteratorsLoopBlock, markVarLoopBodyBlock)

	// mark phase: mark var
	b.SetInsertPoint(markVarLoopBodyBlock, markVarLoopBodyBlock.FirstInstruction())
	markNodeVarPtr := b.CreateGEP(markVarArray, []llvm.Value{markVarIndex}, "")
	markVarNode := b.CreateLoad(markNodeVarPtr, "")
	b.CreateCall(markNodes, []llvm.Value{newGcMark, markVarNode}, "")
	b.CreateBr(markVarLoopBlock)

	// mark phase: iterate over doEdgeIterators in frame
	b.SetInsertPoint(markDoEdgesIteratorsLoopBlock, markDoEdgesIteratorsLoopBlock.FirstInstruction())
	markDoEdgesIteratorsIndex := b.CreatePHI(llvm.Int32Type(), "")
	var markDoEdgesIteratorsNextIndex llvm.Value
	var markIteratorEdgesLoopBlock llvm.BasicBlock
	defer func() {
		markDoEdgesIteratorsIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			markDoEdgesIteratorsNextIndex,
		}, []llvm.BasicBlock{
			markVarLoopBlock,
			markIteratorEdgesLoopBlock,
		})
	}()
	markDoEdgesIteratorsNextIndex = b.CreateAdd(markDoEdgesIteratorsIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	markDoEdgesIteratorsIndexCheck := b.CreateICmp(llvm.IntUGE, markDoEdgesIteratorsIndex, markDoEdgesIteratorsSize, "")
	markDoEdgesIteratorLoopBodyBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(markDoEdgesIteratorsIndexCheck, markFrameLoopBlock, markDoEdgesIteratorLoopBodyBlock)

	// mark phase: set up current doEdgeIterator
	b.SetInsertPoint(markDoEdgesIteratorLoopBodyBlock, markDoEdgesIteratorLoopBodyBlock.FirstInstruction())
	markIteratorEdgesSizePtr := b.CreateGEP(markDoEdgesIteratorsArray, []llvm.Value{
		markDoEdgesIteratorsIndex,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	markIteratorEdgesSize := b.CreateLoad(markIteratorEdgesSizePtr, "")
	markIteratorEdgesArrayPtr := b.CreateGEP(markDoEdgesIteratorsArray, []llvm.Value{
		markDoEdgesIteratorsIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	markIteratorEdgesArray := b.CreateLoad(markIteratorEdgesArrayPtr, "")
	markIteratorEdgesLoopBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(markIteratorEdgesLoopBlock)

	// iterate over edges in current doEdgeIterator
	b.SetInsertPoint(markIteratorEdgesLoopBlock, markIteratorEdgesLoopBlock.FirstInstruction())
	var markIteratorEdgesNextIndex llvm.Value
	var markIteratorEdgesLoopBodyBlock llvm.BasicBlock
	markIteratorEdgesIndex := b.CreatePHI(llvm.Int32Type(), "")
	defer func() {
		markIteratorEdgesIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			markIteratorEdgesNextIndex,
		}, []llvm.BasicBlock{
			markDoEdgesIteratorLoopBodyBlock,
			markIteratorEdgesLoopBodyBlock,
		})
	}()
	markIteratorEdgesNextIndex = b.CreateAdd(markIteratorEdgesIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	markIteratorEdgesIndexCheck := b.CreateICmp(llvm.IntUGE, markIteratorEdgesIndex, markIteratorEdgesSize, "")
	markIteratorEdgesLoopBodyBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(markIteratorEdgesIndexCheck, markDoEdgesIteratorsLoopBlock, markIteratorEdgesLoopBodyBlock)

	b.SetInsertPoint(markIteratorEdgesLoopBodyBlock, markIteratorEdgesLoopBodyBlock.FirstInstruction())
	iteratorEdgePtr := b.CreateGEP(markIteratorEdgesArray, []llvm.Value{markIteratorEdgesIndex}, "")
	iteratorEdge := b.CreateLoad(iteratorEdgePtr, "")
	b.CreateCall(markNodes, []llvm.Value{newGcMark, iteratorEdge}, "")
	b.CreateBr(markIteratorEdgesLoopBlock)

	// sweep phase
	b.SetInsertPoint(startGCSweepBlock, startGCSweepBlock.FirstInstruction())
	sweepPageLoopBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateBr(sweepPageLoopBlock)

	b.SetInsertPoint(sweepPageLoopBlock, sweepPageLoopBlock.FirstInstruction())
	var sweepPageLoopNextIndexBlock llvm.BasicBlock
	var sweepPageLoopNextPageBlock llvm.BasicBlock
	sweepPage := b.CreatePHI(decls.PPageType, "")
	var sweepNextPage llvm.Value
	defer func() {
		sweepPage.AddIncoming([]llvm.Value{
			initialPage,
			sweepPage,
			sweepNextPage,
		}, []llvm.BasicBlock{
			startGCSweepBlock,
			sweepPageLoopNextIndexBlock,
			sweepPageLoopNextPageBlock,
		})
	}()
	sweepIndex := b.CreatePHI(llvm.Int32Type(), "")
	var sweepNextIndex llvm.Value
	defer func() {
		sweepIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			sweepNextIndex,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
		}, []llvm.BasicBlock{
			startGCSweepBlock,
			sweepPageLoopNextIndexBlock,
			sweepPageLoopNextPageBlock,
		})
	}()
	sweepPageLiveCount := b.CreatePHI(llvm.Int32Type(), "")
	var sweepNextPageLiveCount llvm.Value
	defer func() {
		sweepPageLiveCount.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			sweepNextPageLiveCount,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
		}, []llvm.BasicBlock{
			startGCSweepBlock,
			sweepPageLoopNextIndexBlock,
			sweepPageLoopNextPageBlock,
		})
	}()
	compactionEligibleCount := b.CreatePHI(llvm.Int32Type(), "")
	var nextCompactionEligibleCount llvm.Value
	defer func() {
		compactionEligibleCount.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			compactionEligibleCount,
			nextCompactionEligibleCount,
		}, []llvm.BasicBlock{
			startGCSweepBlock,
			sweepPageLoopNextIndexBlock,
			sweepPageLoopNextPageBlock,
		})
	}()
	sweepNodeAlivePtr := b.CreateGEP(sweepPage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		sweepIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	sweepNodeAliveCheck := b.CreateLoad(sweepNodeAlivePtr, "")
	sweepCheckNodeBlock := llvm.AddBasicBlock(decls.NewNode, "")
	sweepPageLoopNextIndexBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(sweepNodeAliveCheck, sweepCheckNodeBlock, sweepPageLoopNextIndexBlock)

	b.SetInsertPoint(sweepCheckNodeBlock, sweepCheckNodeBlock.FirstInstruction())
	sweepGcMarkPtr := b.CreateGEP(sweepPage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		sweepIndex,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	sweepGcMark := b.CreateLoad(sweepGcMarkPtr, "")
	sweepGcMarkCheck := b.CreateICmp(llvm.IntEQ, newGcMark, sweepGcMark, "")
	incrementedPageLiveCount := b.CreateAdd(sweepPageLiveCount, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	sweepCollectNodeBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(sweepGcMarkCheck, sweepPageLoopNextIndexBlock, sweepCollectNodeBlock)

	b.SetInsertPoint(sweepCollectNodeBlock, sweepCollectNodeBlock.FirstInstruction())
	b.CreateStore(llvm.ConstInt(llvm.Int1Type(), 0, false), sweepNodeAlivePtr)
	sweepNodeEdgesSizePtr := b.CreateGEP(sweepPage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		sweepIndex,
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	sweepNodeEdgesSize := b.CreateLoad(sweepNodeEdgesSizePtr, "")
	sweepNodeEdgesSizeCheck := b.CreateICmp(llvm.IntEQ, sweepNodeEdgesSize, llvm.ConstInt(llvm.Int32Type(), 0, false), "")
	sweepCollectNodeClearEdgesBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(sweepNodeEdgesSizeCheck, sweepPageLoopNextIndexBlock, sweepCollectNodeClearEdgesBlock)

	b.SetInsertPoint(sweepCollectNodeClearEdgesBlock, sweepCollectNodeClearEdgesBlock.FirstInstruction())
	sweepNodeEdgesArrayPtr := b.CreateGEP(sweepPage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		sweepIndex,
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	sweepNodeEdgesArray := b.CreateLoad(sweepNodeEdgesArrayPtr, "")
	sweepNodeEdgesRawPtr := b.CreateBitCast(sweepNodeEdgesArray, llvm.PointerType(llvm.Int8Type(), 0), "")
	sweepNodeEdgesSizeofPtr := b.CreateGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{sweepNodeEdgesSize}, "")
	sweepNodeEdgesSizeof := b.CreatePtrToInt(sweepNodeEdgesSizeofPtr, llvm.Int32Type(), "")
	b.CreateCall(decls.Memset, []llvm.Value{
		sweepNodeEdgesRawPtr,
		llvm.ConstInt(llvm.Int8Type(), 0, false),
		sweepNodeEdgesSizeof,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int1Type(), 0, false),
	}, "")
	b.CreateBr(sweepPageLoopNextIndexBlock)

	b.SetInsertPoint(sweepPageLoopNextIndexBlock, sweepPageLoopNextIndexBlock.FirstInstruction())
	sweepNextPageLiveCount = b.CreatePHI(llvm.Int32Type(), "")
	defer func() {
		sweepNextPageLiveCount.AddIncoming([]llvm.Value{
			sweepPageLiveCount,
			incrementedPageLiveCount,
			sweepPageLiveCount,
			sweepPageLiveCount,
		}, []llvm.BasicBlock{
			sweepPageLoopBlock,
			sweepCheckNodeBlock,
			sweepCollectNodeBlock,
			sweepCollectNodeClearEdgesBlock,
		})
	}()
	sweepNextIndex = b.CreateAdd(sweepIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	sweepIndexRangeCheck := b.CreateICmp(llvm.IntUGE, sweepNextIndex, llvm.ConstInt(llvm.Int32Type(), pageSize, false), "")
	checkCompactEligibleBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(sweepIndexRangeCheck, checkCompactEligibleBlock, sweepPageLoopBlock)

	b.SetInsertPoint(checkCompactEligibleBlock, checkCompactEligibleBlock.FirstInstruction())
	compactEligibleCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), pageCompactionThreshold, false), sweepNextPageLiveCount, "")
	isCompactEligibleBlock := llvm.AddBasicBlock(decls.NewNode, "")
	sweepPageLoopNextPageBlock = llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(compactEligibleCheck, isCompactEligibleBlock, sweepPageLoopNextPageBlock)

	b.SetInsertPoint(isCompactEligibleBlock, isCompactEligibleBlock.FirstInstruction())
	incrementedCompactionEligibleCount := b.CreateAdd(compactionEligibleCount, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(sweepPageLoopNextPageBlock)

	b.SetInsertPoint(sweepPageLoopNextPageBlock, sweepPageLoopNextPageBlock.FirstInstruction())
	nextCompactionEligibleCount = b.CreatePHI(llvm.Int32Type(), "")
	nextCompactionEligibleCount.AddIncoming([]llvm.Value{
		compactionEligibleCount,
		incrementedCompactionEligibleCount,
	}, []llvm.BasicBlock{
		checkCompactEligibleBlock,
		isCompactEligibleBlock,
	})
	sweepNextPagePtr := b.CreateGEP(sweepPage, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	sweepNextPage = b.CreateLoad(sweepNextPagePtr, "")
	sweepPageNullCheck := b.CreateICmp(llvm.IntEQ, sweepNextPage, llvm.ConstNull(decls.PPageType), "")
	checkForNewPageBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(sweepPageNullCheck, checkForNewPageBlock, sweepPageLoopBlock)

	b.SetInsertPoint(checkForNewPageBlock, checkForNewPageBlock.FirstInstruction())
	compactionEligibleCountPtr := b.CreateGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	b.CreateStore(nextCompactionEligibleCount, compactionEligibleCountPtr)
	newPageCheck := b.CreateICmp(llvm.IntUGE, sweepNextPageLiveCount, llvm.ConstInt(llvm.Int32Type(), newPageThreshold, false), "")
	newPageBlock := llvm.AddBasicBlock(decls.NewNode, "")
	b.CreateCondBr(newPageCheck, newPageBlock, retryNewNodeBlock)

	b.SetInsertPoint(newPageBlock, newPageBlock.FirstInstruction())
	newPageSizeof := llvm.ConstPtrToInt(llvm.ConstGEP(llvm.ConstNull(decls.PPageType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 1, false)}), llvm.Int32Type())
	newPageRawPtr := b.CreateCall(decls.Malloc, []llvm.Value{newPageSizeof}, "")
	b.CreateCall(decls.Memset, []llvm.Value{
		newPageRawPtr,
		llvm.ConstInt(llvm.Int8Type(), 0, false),
		newPageSizeof,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int1Type(), 0, false),
	}, "")
	newPage := b.CreateBitCast(newPageRawPtr, decls.PPageType, "")
	b.CreateStore(newPage, sweepNextPagePtr)
	b.CreateBr(retryNewNodeBlock)
}

func defMarkNodes(mod llvm.Module, decls *RTDecls) llvm.Value {
	markNodes := llvm.AddFunction(mod, "", llvm.FunctionType(llvm.VoidType(), []llvm.Type{llvm.Int8Type(), decls.PNodeType}, false))

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	gcMark := markNodes.FirstParam()
	nodeParam := llvm.NextParam(gcMark)

	entryBlock := llvm.AddBasicBlock(markNodes, "")
	doneBlock := llvm.AddBasicBlock(markNodes, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	nullCheck := b.CreateICmp(llvm.IntEQ, nodeParam, llvm.ConstNull(decls.PNodeType), "")
	checkAliveBlock := llvm.AddBasicBlock(markNodes, "")
	b.CreateCondBr(nullCheck, doneBlock, checkAliveBlock)

	b.SetInsertPoint(checkAliveBlock, checkAliveBlock.FirstInstruction())
	alivePtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	aliveCheck := b.CreateLoad(alivePtr, "")
	checkGcMarkBlock := llvm.AddBasicBlock(markNodes, "")
	b.CreateCondBr(aliveCheck, checkGcMarkBlock, doneBlock)

	b.SetInsertPoint(checkGcMarkBlock, checkGcMarkBlock.FirstInstruction())
	nodeGcMarkPtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	nodeGcMark := b.CreateLoad(nodeGcMarkPtr, "")
	nodeGcMarkCheck := b.CreateICmp(llvm.IntEQ, nodeGcMark, gcMark, "")
	checkEdgesInitBlock := llvm.AddBasicBlock(markNodes, "")
	b.CreateCondBr(nodeGcMarkCheck, doneBlock, checkEdgesInitBlock)

	b.SetInsertPoint(checkEdgesInitBlock, checkEdgesInitBlock.FirstInstruction())
	b.CreateStore(gcMark, nodeGcMarkPtr)
	nodeEdgesArraySizePtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	nodeEdgesArraySize := b.CreateLoad(nodeEdgesArraySizePtr, "")
	nodeEdgesArrayPtr := b.CreateGEP(nodeParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	nodeEdgesArray := b.CreateLoad(nodeEdgesArrayPtr, "")
	checkEdgesLoopBlock := llvm.AddBasicBlock(markNodes, "")
	b.CreateBr(checkEdgesLoopBlock)

	b.SetInsertPoint(checkEdgesLoopBlock, checkEdgesLoopBlock.FirstInstruction())
	checkEdgesLoopBodyBlock := llvm.AddBasicBlock(markNodes, "")
	edgeIndex := b.CreatePHI(llvm.Int32Type(), "")
	nextEdgeIndex := b.CreateAdd(edgeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	edgeIndex.AddIncoming([]llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		nextEdgeIndex,
	}, []llvm.BasicBlock{
		checkEdgesInitBlock,
		checkEdgesLoopBodyBlock,
	})
	edgeIndexCheck := b.CreateICmp(llvm.IntUGE, edgeIndex, nodeEdgesArraySize, "")
	b.CreateCondBr(edgeIndexCheck, doneBlock, checkEdgesLoopBodyBlock)

	b.SetInsertPoint(checkEdgesLoopBodyBlock, checkEdgesLoopBodyBlock.FirstInstruction())
	edgeElementPtr := b.CreateGEP(nodeEdgesArray, []llvm.Value{edgeIndex}, "")
	edgeElement := b.CreateLoad(edgeElementPtr, "")
	b.CreateCall(markNodes, []llvm.Value{gcMark, edgeElement}, "")
	b.CreateBr(checkEdgesLoopBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()

	return markNodes
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
	moveNodes := defMoveNodes(mod, decls)

	b := mod.Context().NewBuilder()
	defer b.Dispose()
	entryBlock := llvm.AddBasicBlock(decls.Compact, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	//...
	_ = moveNodes
	b.CreateRetVoid()
}

func defMoveNodes(mod llvm.Module, decls *RTDecls) llvm.Value {
	moveNodeReferencesInEdgeArray := defMoveNodeReferencesInEdgeArray(mod, decls)

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	moveNodes := llvm.AddFunction(mod, "", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType, llvm.Int32Type(), decls.PPNodeType}, false))

	// nodeCount is size of nodeArray
	// every other element of nodeArray is the node to be moved
	// interspersed with the node destination
	frameParam := moveNodes.FirstParam()
	nodeCountParam := llvm.NextParam(frameParam)
	nodeArrayParam := llvm.NextParam(nodeCountParam)

	// first, move the actual nodes in the pages
	entryBlock := llvm.AddBasicBlock(moveNodes, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	initialPage := llvm.ConstGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	})
	moveActualNodesLoopBlock := llvm.AddBasicBlock(moveNodes, "")
	b.CreateBr(moveActualNodesLoopBlock)

	b.SetInsertPoint(moveActualNodesLoopBlock, moveActualNodesLoopBlock.FirstInstruction())
	moveActualNodesIndex := b.CreatePHI(llvm.Int32Type(), "")
	var moveActualNodesNextIndex llvm.Value
	var moveActualNodesLoopBodyBlock llvm.BasicBlock
	defer func() {
		moveActualNodesIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			moveActualNodesNextIndex,
		}, []llvm.BasicBlock{
			entryBlock,
			moveActualNodesLoopBodyBlock,
		})
	}()
	moveActualNodesIndexP1 := b.CreateAdd(moveActualNodesIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	moveActualNodesNextIndex = b.CreateAdd(moveActualNodesIndex, llvm.ConstInt(llvm.Int32Type(), 2, false), "")
	moveActualNodesIndexCheck := b.CreateICmp(llvm.IntUGE, moveActualNodesIndex, nodeCountParam, "")
	moveActualNodesLoopBodyBlock = llvm.AddBasicBlock(moveNodes, "")
	pageLoopBlock := llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(moveActualNodesIndexCheck, pageLoopBlock, moveActualNodesLoopBodyBlock)

	b.SetInsertPoint(moveActualNodesLoopBodyBlock, moveActualNodesLoopBodyBlock.FirstInstruction())
	srcNodePtr := b.CreateGEP(nodeArrayParam, []llvm.Value{moveActualNodesIndex}, "")
	srcNode := b.CreateLoad(srcNodePtr, "")
	destNodePtr := b.CreateGEP(nodeArrayParam, []llvm.Value{moveActualNodesIndexP1}, "")
	destNode := b.CreateLoad(destNodePtr, "")
	moveActualNode(decls, b, srcNode, destNode)
	b.CreateBr(moveActualNodesLoopBlock)

	// move the edges for all the nodes in all the pages
	b.SetInsertPoint(pageLoopBlock, pageLoopBlock.FirstInstruction())
	page := b.CreatePHI(decls.PPageType, "")
	var nextPage llvm.Value
	var pageNodeLoopBlock llvm.BasicBlock
	defer func() {
		page.AddIncoming([]llvm.Value{
			initialPage,
			nextPage,
		}, []llvm.BasicBlock{
			moveActualNodesLoopBlock,
			pageNodeLoopBlock,
		})
	}()
	pageCheck := b.CreateICmp(llvm.IntEQ, page, llvm.ConstNull(decls.PPageType), "")
	pageLoopBodyBlock := llvm.AddBasicBlock(moveNodes, "")
	frameLoopBlock := llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(pageCheck, frameLoopBlock, pageLoopBodyBlock)

	b.SetInsertPoint(pageLoopBodyBlock, pageLoopBodyBlock.FirstInstruction())
	nextPagePtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	nextPage = b.CreateLoad(nextPagePtr, "")
	pageNodeLoopBlock = llvm.AddBasicBlock(moveNodes, "")
	b.CreateBr(pageNodeLoopBlock)

	b.SetInsertPoint(pageNodeLoopBlock, pageNodeLoopBlock.FirstInstruction())
	pageNodeIndex := b.CreatePHI(llvm.Int32Type(), "")
	var pageNodeNextIndex llvm.Value
	var pageNodeLoopBodyBlock llvm.BasicBlock
	var pageNodeMoveEdgesBlock llvm.BasicBlock
	defer func() {
		pageNodeIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			pageNodeNextIndex,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
		}, []llvm.BasicBlock{
			pageLoopBodyBlock,
			pageNodeLoopBodyBlock,
			pageNodeMoveEdgesBlock,
		})
	}()
	pageNodeNextIndex = b.CreateAdd(pageNodeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	pageNodeIndexCheck := b.CreateICmp(llvm.IntUGE, pageNodeIndex, llvm.ConstInt(llvm.Int32Type(), pageSize, false), "")
	pageNodeLoopBodyBlock = llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(pageNodeIndexCheck, pageLoopBlock, pageNodeLoopBodyBlock)

	b.SetInsertPoint(pageNodeLoopBodyBlock, pageNodeLoopBodyBlock.FirstInstruction())
	pageNodeLivePtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		pageNodeIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	pageNodeLive := b.CreateLoad(pageNodeLivePtr, "")
	pageNodeMoveEdgesBlock = llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(pageNodeLive, pageNodeMoveEdgesBlock, pageNodeLoopBlock)

	b.SetInsertPoint(pageNodeMoveEdgesBlock, pageNodeMoveEdgesBlock.FirstInstruction())
	pageNodeEdgesArraySizePtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		pageNodeIndex,
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	pageNodeEdgesArraySize := b.CreateLoad(pageNodeEdgesArraySizePtr, "")
	pageNodeEdgesArrayPtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		pageNodeIndex,
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	pageNodeEdgesArray := b.CreateLoad(pageNodeEdgesArrayPtr, "")
	b.CreateCall(moveNodeReferencesInEdgeArray, []llvm.Value{
		pageNodeEdgesArraySize,
		pageNodeEdgesArray,
		nodeCountParam,
		nodeArrayParam,
	}, "")
	b.CreateBr(pageNodeLoopBlock)

	// for each frame, move all the local variables, then all the edges
	// in all the doEdgesIterators
	b.SetInsertPoint(frameLoopBlock, frameLoopBlock.FirstInstruction())
	currentFrame := b.CreatePHI(decls.PFrameType, "")
	var nextFrame llvm.Value
	var doEdgesLoopBlock llvm.BasicBlock
	defer func() {
		currentFrame.AddIncoming([]llvm.Value{
			frameParam,
			nextFrame,
		}, []llvm.BasicBlock{
			pageLoopBlock,
			doEdgesLoopBlock,
		})
	}()
	currentFrameCheck := b.CreateICmp(llvm.IntEQ, currentFrame, llvm.ConstNull(decls.PFrameType), "")
	frameLoopBodyBlock := llvm.AddBasicBlock(moveNodes, "")
	doneBlock := llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(currentFrameCheck, doneBlock, frameLoopBodyBlock)

	b.SetInsertPoint(frameLoopBodyBlock, frameLoopBodyBlock.FirstInstruction())
	nextFramePtr := b.CreateGEP(currentFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	nextFrame = b.CreateLoad(nextFramePtr, "")
	varArraySizePtr := b.CreateGEP(currentFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	varArraySize := b.CreateLoad(varArraySizePtr, "")
	varArrayPtr := b.CreateGEP(currentFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	varArray := b.CreateLoad(varArrayPtr, "")
	b.CreateCall(moveNodeReferencesInEdgeArray, []llvm.Value{
		varArraySize,
		varArray,
		nodeCountParam,
		nodeArrayParam,
	}, "")
	doEdgesArraySizePtr := b.CreateGEP(currentFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	doEdgesArraySize := b.CreateLoad(doEdgesArraySizePtr, "")
	doEdgesArrayPtr := b.CreateGEP(currentFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 4, false),
	}, "")
	doEdgesArray := b.CreateLoad(doEdgesArrayPtr, "")
	doEdgesLoopBlock = llvm.AddBasicBlock(moveNodes, "")
	b.CreateBr(doEdgesLoopBlock)

	b.SetInsertPoint(doEdgesLoopBlock, doEdgesLoopBlock.FirstInstruction())
	doEdgesIndex := b.CreatePHI(llvm.Int32Type(), "")
	var doEdgesNextIndex llvm.Value
	var doEdgesLoopBodyBlock llvm.BasicBlock
	defer func() {
		doEdgesIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			doEdgesNextIndex,
		}, []llvm.BasicBlock{
			frameLoopBodyBlock,
			doEdgesLoopBodyBlock,
		})
	}()
	doEdgesNextIndex = b.CreateAdd(doEdgesIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	doEdgesIndexCheck := b.CreateICmp(llvm.IntUGE, doEdgesIndex, doEdgesArraySize, "")
	doEdgesLoopBodyBlock = llvm.AddBasicBlock(moveNodes, "")
	b.CreateCondBr(doEdgesIndexCheck, frameLoopBlock, doEdgesLoopBodyBlock)

	b.SetInsertPoint(doEdgesLoopBodyBlock, doEdgesLoopBodyBlock.FirstInstruction())
	doEdgesEdgesArraySizePtr := b.CreateGEP(doEdgesArray, []llvm.Value{
		doEdgesIndex,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	doEdgesEdgesArraySize := b.CreateLoad(doEdgesEdgesArraySizePtr, "")
	doEdgesEdgesArrayPtr := b.CreateGEP(doEdgesArray, []llvm.Value{
		doEdgesIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	doEdgesEdgesArray := b.CreateLoad(doEdgesEdgesArrayPtr, "")
	b.CreateCall(moveNodeReferencesInEdgeArray, []llvm.Value{
		doEdgesEdgesArraySize,
		doEdgesEdgesArray,
		nodeCountParam,
		nodeArrayParam,
	}, "")
	b.CreateBr(doEdgesLoopBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()

	return moveNodes
}

func defMoveNodeReferencesInEdgeArray(mod llvm.Module, decls *RTDecls) llvm.Value {
	b := mod.Context().NewBuilder()
	defer b.Dispose()

	moveNodeReferencesInEdgeArray := llvm.AddFunction(mod, "", llvm.FunctionType(llvm.VoidType(), []llvm.Type{llvm.Int32Type(), decls.PPNodeType, llvm.Int32Type(), decls.PPNodeType}, false))

	// nodeCount is size of nodeArray
	// every other element of nodeArray is the node to be moved
	// interspersed with the node destination
	edgeCountParam := moveNodeReferencesInEdgeArray.FirstParam()
	edgeArrayParam := llvm.NextParam(edgeCountParam)
	nodeCountParam := llvm.NextParam(edgeArrayParam)
	nodeArrayParam := llvm.NextParam(nodeCountParam)

	entryBlock := llvm.AddBasicBlock(moveNodeReferencesInEdgeArray, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	edgeLoopBlock := llvm.AddBasicBlock(moveNodeReferencesInEdgeArray, "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(edgeLoopBlock, edgeLoopBlock.FirstInstruction())
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	var edgeLoopBodyEndBlock llvm.BasicBlock
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
		}, []llvm.BasicBlock{
			entryBlock,
			edgeLoopBodyEndBlock,
		})
	}()
	indexCheck := b.CreateICmp(llvm.IntUGE, index, edgeCountParam, "")
	edgeLoopBodyBlock := llvm.AddBasicBlock(moveNodeReferencesInEdgeArray, "")
	doneBlock := llvm.AddBasicBlock(moveNodeReferencesInEdgeArray, "")
	b.CreateCondBr(indexCheck, doneBlock, edgeLoopBodyBlock)

	b.SetInsertPoint(edgeLoopBodyBlock, edgeLoopBodyBlock.FirstInstruction())
	edgeReference := b.CreateGEP(edgeArrayParam, []llvm.Value{index}, "")
	edgeLoopBodyEndBlock = llvm.AddBasicBlock(moveNodeReferencesInEdgeArray, "")
	moveNodeReference(decls, moveNodeReferencesInEdgeArray, b, edgeReference, nodeCountParam, nodeArrayParam, edgeLoopBodyBlock, edgeLoopBodyEndBlock)

	b.SetInsertPoint(edgeLoopBodyEndBlock, edgeLoopBodyEndBlock.FirstInstruction())
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()

	return moveNodeReferencesInEdgeArray
}

func moveActualNode(decls *RTDecls, b llvm.Builder, srcNode, destNode llvm.Value) {
	srcLivePtr := b.CreateGEP(srcNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	srcEdgeArraySizePtr := b.CreateGEP(srcNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	srcEdgeArrayPtr := b.CreateGEP(srcNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	destLivePtr := b.CreateGEP(destNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	destEdgeArraySizePtr := b.CreateGEP(destNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	destEdgeArrayPtr := b.CreateGEP(destNode, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	srcEdgeArraySize := b.CreateLoad(srcEdgeArraySizePtr, "")
	srcEdgeArray := b.CreateLoad(srcEdgeArrayPtr, "")
	destEdgeArraySize := b.CreateLoad(destEdgeArraySizePtr, "")
	destEdgeArray := b.CreateLoad(destEdgeArrayPtr, "")
	b.CreateStore(llvm.ConstInt(llvm.Int1Type(), 0, false), srcLivePtr)
	b.CreateStore(destEdgeArraySize, srcEdgeArraySizePtr)
	b.CreateStore(destEdgeArray, srcEdgeArrayPtr)
	b.CreateStore(llvm.ConstInt(llvm.Int1Type(), 1, false), destLivePtr)
	b.CreateStore(srcEdgeArraySize, destEdgeArraySizePtr)
	b.CreateStore(srcEdgeArray, destEdgeArrayPtr)
}

func moveNodeReference(decls *RTDecls, function llvm.Value, b llvm.Builder, nodeReference, nodeCountParam, nodeArrayParam llvm.Value, entryBlock, exitBlock llvm.BasicBlock) {
	// nodeCount is size of nodeArray
	// every other element of nodeArray is the node to be moved
	// interspersed with the node destination
	node := b.CreateLoad(nodeReference, "")
	nodeCheck := b.CreateICmp(llvm.IntEQ, node, llvm.ConstNull(decls.PNodeType), "")
	nodeArrayLoopBlock := llvm.AddBasicBlock(function, "")
	b.CreateCondBr(nodeCheck, exitBlock, nodeArrayLoopBlock)

	b.SetInsertPoint(nodeArrayLoopBlock, nodeArrayLoopBlock.FirstInstruction())
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	var nodeArrayLoopBodyBlock llvm.BasicBlock
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
		}, []llvm.BasicBlock{
			entryBlock,
			nodeArrayLoopBodyBlock,
		})
	}()
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 2, false), "")
	indexCheck := b.CreateICmp(llvm.IntUGE, index, nodeCountParam, "")
	nodeArrayLoopBodyBlock = llvm.AddBasicBlock(function, "")
	b.CreateCondBr(indexCheck, exitBlock, nodeArrayLoopBodyBlock)

	b.SetInsertPoint(nodeArrayLoopBodyBlock, nodeArrayLoopBodyBlock.FirstInstruction())
	srcNodePtr := b.CreateGEP(nodeArrayParam, []llvm.Value{index}, "")
	srcNode := b.CreateLoad(srcNodePtr, "")
	srcNodeCheck := b.CreateICmp(llvm.IntEQ, srcNode, llvm.ConstNull(decls.PNodeType), "")
	moveNodeBlock := llvm.AddBasicBlock(function, "")
	b.CreateCondBr(srcNodeCheck, moveNodeBlock, nodeArrayLoopBlock)

	b.SetInsertPoint(moveNodeBlock, moveNodeBlock.FirstInstruction())
	indexP1 := b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	destNodePtr := b.CreateGEP(nodeArrayParam, []llvm.Value{indexP1}, "")
	destNode := b.CreateLoad(destNodePtr, "")
	b.CreateStore(destNode, nodeReference)
	b.CreateBr(exitBlock)
}
