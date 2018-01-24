package main

import (
	"llvm.org/llvm/bindings/go/llvm"
)

func AddDGOLLib(mod llvm.Module, decls *RTDecls, lib string) {
	switch lib {
	case "IO":
		addIOLib(mod, decls)
	case "DEBUGTRACE":
		addDEBUGTRACELib(mod, decls)
	default:
	}
}

func addIOLib(mod llvm.Module, decls *RTDecls) {
	const readFuncName = "read"
	readFunc := mod.NamedFunction(readFuncName)
	if readFunc.IsNil() {
		readFunc = llvm.AddFunction(mod, readFuncName, llvm.FunctionType(llvm.Int32Type(), []llvm.Type{llvm.Int32Type(), llvm.PointerType(llvm.Int8Type(), 0), llvm.Int32Type()}, false))
	}
	const writeFuncName = "write"
	writeFunc := mod.NamedFunction(writeFuncName)
	if writeFunc.IsNil() {
		writeFunc = llvm.AddFunction(mod, writeFuncName, llvm.FunctionType(llvm.Int32Type(), []llvm.Type{llvm.Int32Type(), llvm.PointerType(llvm.Int8Type(), 0), llvm.Int32Type()}, false))
	}
	addIOLibREADBYTE(mod, decls, readFunc)
	addIOLibWRITEBYTE(mod, decls, writeFunc)
}

func addIOLibREADBYTE(mod llvm.Module, decls *RTDecls, readFunc llvm.Value) {
	fn := llvm.AddFunction(mod, "IO.READBYTE", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")
	doneBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	argCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	argCount := b.CreateLoad(argCountPtr, "")
	argArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	argArray := b.CreateLoad(argArrayPtr, "")
	buffer := b.CreateAlloca(llvm.Int8Type(), "")
	readCount := b.CreateCall(readFunc, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		buffer,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")

	arg0IndexCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), 0, false), argCount, "")
	arg0CheckBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(arg0IndexCheck, doneBlock, arg0CheckBlock)

	b.SetInsertPoint(arg0CheckBlock, arg0CheckBlock.FirstInstruction())
	arg0Ptr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, false)}, "")
	arg0 := b.CreateLoad(arg0Ptr, "")
	arg0Check := b.CreateICmp(llvm.IntEQ, arg0, llvm.ConstNull(decls.PPNodeType), "")
	node0CheckBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(arg0Check, doneBlock, node0CheckBlock)

	b.SetInsertPoint(node0CheckBlock, node0CheckBlock.FirstInstruction())
	initialNode0 := b.CreateLoad(arg0, "")
	node0Check := b.CreateICmp(llvm.IntEQ, initialNode0, llvm.ConstNull(decls.PNodeType), "")
	newNode0Block := llvm.AddBasicBlock(fn, "")
	node0Block := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(node0Check, newNode0Block, node0Block)

	b.SetInsertPoint(newNode0Block, newNode0Block.FirstInstruction())
	newNode0 := b.CreateCall(decls.NewNode, []llvm.Value{frameParam}, "")
	b.CreateStore(newNode0, arg0)
	b.CreateBr(node0Block)

	b.SetInsertPoint(node0Block, node0Block.FirstInstruction())
	node0 := b.CreatePHI(decls.PNodeType, "")
	node0.AddIncoming([]llvm.Value{
		initialNode0,
		newNode0,
	}, []llvm.BasicBlock{
		node0CheckBlock,
		newNode0Block,
	})
	eofCheckBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(eofCheckBlock)

	addEdge := func(index int, block, nextBlock llvm.BasicBlock) {
		b.SetInsertPoint(block, block.FirstInstruction())
		argIndexCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), uint64(index), false), argCount, "")
		argCheckBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(argIndexCheck, doneBlock, argCheckBlock)

		b.SetInsertPoint(argCheckBlock, argCheckBlock.FirstInstruction())
		argPtr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(index), false)}, "")
		arg := b.CreateLoad(argPtr, "")
		argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
		checkNodeBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(argCheck, nextBlock, checkNodeBlock)

		b.SetInsertPoint(checkNodeBlock, checkNodeBlock.FirstInstruction())
		initialNode := b.CreateLoad(arg, "")
		nodeCheck := b.CreateICmp(llvm.IntEQ, initialNode, llvm.ConstNull(decls.PNodeType), "")
		newNodeBlock := llvm.AddBasicBlock(fn, "")
		addEdgeBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(nodeCheck, newNodeBlock, addEdgeBlock)

		b.SetInsertPoint(newNodeBlock, newNodeBlock.FirstInstruction())
		newNode := b.CreateCall(decls.NewNode, []llvm.Value{frameParam}, "")
		b.CreateStore(newNode, arg)
		b.CreateBr(addEdgeBlock)

		b.SetInsertPoint(addEdgeBlock, addEdgeBlock.FirstInstruction())
		node := b.CreatePHI(decls.PNodeType, "")
		node.AddIncoming([]llvm.Value{
			initialNode,
			newNode,
		}, []llvm.BasicBlock{
			checkNodeBlock,
			newNodeBlock,
		})
		b.CreateCall(decls.AddEdge, []llvm.Value{node0, node}, "")
		b.CreateBr(nextBlock)
	}

	rmEdge := func(index int, block, nextBlock llvm.BasicBlock) {
		b.SetInsertPoint(block, block.FirstInstruction())
		argIndexCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), uint64(index), false), argCount, "")
		argCheckBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(argIndexCheck, doneBlock, argCheckBlock)

		b.SetInsertPoint(argCheckBlock, argCheckBlock.FirstInstruction())
		argPtr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(index), false)}, "")
		arg := b.CreateLoad(argPtr, "")
		argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
		checkNodeBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(argCheck, nextBlock, checkNodeBlock)

		b.SetInsertPoint(checkNodeBlock, checkNodeBlock.FirstInstruction())
		node := b.CreateLoad(arg, "")
		nodeCheck := b.CreateICmp(llvm.IntEQ, node, llvm.ConstNull(decls.PNodeType), "")
		rmEdgeBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(nodeCheck, nextBlock, rmEdgeBlock)

		b.SetInsertPoint(rmEdgeBlock, rmEdgeBlock.FirstInstruction())
		b.CreateCall(decls.RemoveEdge, []llvm.Value{node0, node}, "")
		b.CreateBr(nextBlock)
	}

	b.SetInsertPoint(eofCheckBlock, eofCheckBlock.FirstInstruction())
	eofCheck := b.CreateICmp(llvm.IntEQ, readCount, llvm.ConstInt(llvm.Int32Type(), 0, false), "")
	isEOFBlock := llvm.AddBasicBlock(fn, "")
	notEOFBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(eofCheck, isEOFBlock, notEOFBlock)

	nextBlock := llvm.AddBasicBlock(fn, "")
	addEdge(1, isEOFBlock, doneBlock)
	rmEdge(1, notEOFBlock, nextBlock)

	b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
	nextBlock = llvm.AddBasicBlock(fn, "")
	byte := b.CreateLoad(buffer, "")
	b.CreateBr(nextBlock)

	for i := 0; i < 8; i++ {
		b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
		bit := b.CreateAnd(byte, llvm.ConstInt(llvm.Int8Type(), uint64(1<<uint(i)), false), "")
		bitCheck := b.CreateICmp(llvm.IntEQ, llvm.ConstInt(llvm.Int8Type(), 0, false), bit, "")
		rmEdgeBlock := llvm.AddBasicBlock(fn, "")
		addEdgeBlock := llvm.AddBasicBlock(fn, "")
		b.CreateCondBr(bitCheck, rmEdgeBlock, addEdgeBlock)

		nextBlock = llvm.AddBasicBlock(fn, "")
		addEdge(i+2, addEdgeBlock, nextBlock)
		rmEdge(i+2, rmEdgeBlock, nextBlock)
	}
	b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
	b.CreateBr(doneBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()
}

func addIOLibWRITEBYTE(mod llvm.Module, decls *RTDecls, writeFunc llvm.Value) {
	fn := llvm.AddFunction(mod, "IO.WRITEBYTE", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")
	doneBlock := llvm.AddBasicBlock(fn, "")
	doneValues := []llvm.Value{}
	donePreds := []llvm.BasicBlock{}

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	argCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	argCount := b.CreateLoad(argCountPtr, "")
	argArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	argArray := b.CreateLoad(argArrayPtr, "")
	arg0IndexCheck := b.CreateICmp(llvm.IntEQ, llvm.ConstInt(llvm.Int32Type(), 0, false), argCount, "")
	arg0CheckBlock := llvm.AddBasicBlock(fn, "")
	doneValues = append(doneValues, llvm.ConstInt(llvm.Int8Type(), 0, false))
	donePreds = append(donePreds, entryBlock)
	b.CreateCondBr(arg0IndexCheck, doneBlock, arg0CheckBlock)

	b.SetInsertPoint(arg0CheckBlock, arg0CheckBlock.FirstInstruction())
	arg0Ptr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, false)}, "")
	arg0 := b.CreateLoad(arg0Ptr, "")
	arg0Check := b.CreateICmp(llvm.IntEQ, arg0, llvm.ConstNull(decls.PPNodeType), "")
	node0CheckBlock := llvm.AddBasicBlock(fn, "")
	doneValues = append(doneValues, llvm.ConstInt(llvm.Int8Type(), 0, false))
	donePreds = append(donePreds, arg0CheckBlock)
	b.CreateCondBr(arg0Check, doneBlock, node0CheckBlock)

	b.SetInsertPoint(node0CheckBlock, node0CheckBlock.FirstInstruction())
	node0 := b.CreateLoad(arg0, "")
	node0Check := b.CreateICmp(llvm.IntEQ, node0, llvm.ConstNull(decls.PNodeType), "")
	nextBlock := llvm.AddBasicBlock(fn, "")
	nextValues := []llvm.Value{llvm.ConstInt(llvm.Int8Type(), 0, false)}
	nextPreds := []llvm.BasicBlock{node0CheckBlock}
	doneValues = append(doneValues, llvm.ConstInt(llvm.Int8Type(), 0, false))
	donePreds = append(donePreds, node0CheckBlock)
	b.CreateCondBr(node0Check, doneBlock, nextBlock)

	for i := 0; i < 8; i++ {
		currentBlock := nextBlock
		nextBlock = llvm.AddBasicBlock(fn, "")

		b.SetInsertPoint(currentBlock, currentBlock.FirstInstruction())
		value := b.CreatePHI(llvm.Int8Type(), "")
		value.AddIncoming(nextValues, nextPreds)
		nextValues = []llvm.Value{}
		nextPreds = []llvm.BasicBlock{}

		argIndexCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), uint64(i+1), false), argCount, "")
		argCheckBlock := llvm.AddBasicBlock(fn, "")
		doneValues = append(doneValues, value)
		donePreds = append(donePreds, currentBlock)
		b.CreateCondBr(argIndexCheck, doneBlock, argCheckBlock)

		b.SetInsertPoint(argCheckBlock, argCheckBlock.FirstInstruction())
		argPtr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(i+1), false)}, "")
		arg := b.CreateLoad(argPtr, "")
		argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
		checkNodeBlock := llvm.AddBasicBlock(fn, "")
		nextValues = append(nextValues, value)
		nextPreds = append(nextPreds, argCheckBlock)
		b.CreateCondBr(argCheck, nextBlock, checkNodeBlock)

		b.SetInsertPoint(checkNodeBlock, checkNodeBlock.FirstInstruction())
		node := b.CreateLoad(arg, "")
		nodeCheck := b.CreateICmp(llvm.IntEQ, node, llvm.ConstNull(decls.PNodeType), "")
		checkEdgeBlock := llvm.AddBasicBlock(fn, "")
		nextValues = append(nextValues, value)
		nextPreds = append(nextPreds, checkNodeBlock)
		b.CreateCondBr(nodeCheck, nextBlock, checkEdgeBlock)

		b.SetInsertPoint(checkEdgeBlock, checkEdgeBlock.FirstInstruction())
		hasEdgeCheck := b.CreateCall(decls.HasEdge, []llvm.Value{node0, node}, "")
		setBitBlock := llvm.AddBasicBlock(fn, "")
		nextValues = append(nextValues, value)
		nextPreds = append(nextPreds, checkEdgeBlock)
		b.CreateCondBr(hasEdgeCheck, setBitBlock, nextBlock)

		b.SetInsertPoint(setBitBlock, setBitBlock.FirstInstruction())
		newValue := b.CreateOr(value, llvm.ConstInt(llvm.Int8Type(), 1<<uint64(i), false), "")
		nextValues = append(nextValues, newValue)
		nextPreds = append(nextPreds, setBitBlock)
		b.CreateBr(nextBlock)
	}
	b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
	finalValue := b.CreatePHI(llvm.Int8Type(), "")
	finalValue.AddIncoming(nextValues, nextPreds)
	doneValues = append(doneValues, finalValue)
	donePreds = append(donePreds, nextBlock)
	b.CreateBr(doneBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	byte := b.CreatePHI(llvm.Int8Type(), "")
	byte.AddIncoming(doneValues, donePreds)
	buffer := b.CreateAlloca(llvm.Int8Type(), "")
	b.CreateStore(byte, buffer)
	b.CreateCall(writeFunc, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		buffer,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	b.CreateRetVoid()
}

func addDEBUGTRACELib(mod llvm.Module, decls *RTDecls) {
	printfFunction := mod.NamedFunction("printf")
	if printfFunction.IsNil() {
		printfFunction = llvm.AddFunction(mod, "printf", llvm.FunctionType(llvm.VoidType(), []llvm.Type{llvm.PointerType(llvm.Int8Type(), 0)}, true))
	}

	printf := func(b llvm.Builder, format string, args ...llvm.Value) {
		formatPtr := b.CreateBitCast(b.CreateGlobalStringPtr(format, ""), llvm.PointerType(llvm.Int8Type(), 0), "")
		args = append([]llvm.Value{formatPtr}, args...)
		b.CreateCall(printfFunction, args, "")
	}

	addDEBUGTRACELibLABEL(mod, decls, printf)
	addDEBUGTRACELibNODE(mod, decls, printf)
	addDEBUGTRACELibFRAME(mod, decls, printf)
	addDEBUGTRACELibARGS(mod, decls, printf)
	addDEBUGTRACELibPAGES(mod, decls, printf)
}

func addDEBUGTRACELibLABEL(mod llvm.Module, decls *RTDecls, printf func(llvm.Builder, string, ...llvm.Value)) {
	fn := llvm.AddFunction(mod, "DEBUGTRACE.LABEL", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	argCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	argCount := b.CreateLoad(argCountPtr, "")
	argArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	argArray := b.CreateLoad(argArrayPtr, "")
	loopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(loopBlock)

	b.SetInsertPoint(loopBlock, loopBlock.FirstInstruction())
	label := b.CreatePHI(llvm.Int32Type(), "")
	var nextLabel llvm.Value
	var testIndexBlock llvm.BasicBlock
	var addBitBlock llvm.BasicBlock
	defer func() {
		label.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			label,
			nextLabel,
		}, []llvm.BasicBlock{
			entryBlock,
			testIndexBlock,
			addBitBlock,
		})
	}()
	bit := b.CreatePHI(llvm.Int32Type(), "")
	var nextBit llvm.Value
	defer func() {
		bit.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 1, false),
			nextBit,
			nextBit,
		}, []llvm.BasicBlock{
			entryBlock,
			testIndexBlock,
			addBitBlock,
		})
	}()
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
			nextIndex,
		}, []llvm.BasicBlock{
			entryBlock,
			testIndexBlock,
			addBitBlock,
		})
	}()
	indexCheck := b.CreateICmp(llvm.IntUGE, index, argCount, "")
	testIndexBlock = llvm.AddBasicBlock(fn, "")
	addBitBlock = llvm.AddBasicBlock(fn, "")
	doneBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(indexCheck, doneBlock, testIndexBlock)

	b.SetInsertPoint(testIndexBlock, testIndexBlock.FirstInstruction())
	nextBit = b.CreateShl(bit, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	argPtr := b.CreateGEP(argArray, []llvm.Value{index}, "")
	arg := b.CreateLoad(argPtr, "")
	argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
	b.CreateCondBr(argCheck, loopBlock, addBitBlock)

	b.SetInsertPoint(addBitBlock, addBitBlock.FirstInstruction())
	nextLabel = b.CreateAdd(label, bit, "")
	b.CreateBr(loopBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	printf(b, "LABEL %d\n", label)
	b.CreateRetVoid()
}

func addDEBUGTRACELibNODE(mod llvm.Module, decls *RTDecls, printf func(llvm.Builder, string, ...llvm.Value)) {
	fn := llvm.AddFunction(mod, "DEBUGTRACE.NODE", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	argCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	argCount := b.CreateLoad(argCountPtr, "")
	argCountCheck := b.CreateICmp(llvm.IntEQ, argCount, llvm.ConstInt(llvm.Int32Type(), 0, false), "")
	printNullBlock := llvm.AddBasicBlock(fn, "")
	checkArgNullBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(argCountCheck, printNullBlock, checkArgNullBlock)

	b.SetInsertPoint(printNullBlock, printNullBlock.FirstInstruction())
	printf(b, "NODE NULL\n")
	b.CreateRetVoid()

	b.SetInsertPoint(checkArgNullBlock, checkArgNullBlock.FirstInstruction())
	argArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	argArray := b.CreateLoad(argArrayPtr, "")
	argPtr := b.CreateGEP(argArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), 0, false)}, "")
	arg := b.CreateLoad(argPtr, "")
	argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
	checkNodeNullBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(argCheck, printNullBlock, checkNodeNullBlock)

	b.SetInsertPoint(checkNodeNullBlock, checkNodeNullBlock.FirstInstruction())
	node := b.CreateLoad(arg, "")
	nodeCheck := b.CreateICmp(llvm.IntEQ, node, llvm.ConstNull(decls.PNodeType), "")
	initPrintNodeBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(nodeCheck, printNullBlock, initPrintNodeBlock)

	b.SetInsertPoint(initPrintNodeBlock, initPrintNodeBlock.FirstInstruction())
	edgeArraySizePtr := b.CreateGEP(node, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	edgeArraySize := b.CreateLoad(edgeArraySizePtr, "")
	edgeArrayPtr := b.CreateGEP(node, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	edgeArray := b.CreateLoad(edgeArrayPtr, "")
	printf(b, "NODE %p EDGES[%d]", node, edgeArraySize)
	loopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(loopBlock)

	b.SetInsertPoint(loopBlock, loopBlock.FirstInstruction())
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	var loopBodyBlock llvm.BasicBlock
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
		}, []llvm.BasicBlock{
			initPrintNodeBlock,
			loopBodyBlock,
		})
	}()
	indexCheck := b.CreateICmp(llvm.IntUGE, index, edgeArraySize, "")
	loopBodyBlock = llvm.AddBasicBlock(fn, "")
	loopDoneBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(indexCheck, loopDoneBlock, loopBodyBlock)

	b.SetInsertPoint(loopBodyBlock, loopBodyBlock.FirstInstruction())
	edgePtr := b.CreateGEP(edgeArray, []llvm.Value{index}, "")
	edge := b.CreateLoad(edgePtr, "")
	printf(b, " %p", edge)
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(loopBlock)

	b.SetInsertPoint(loopDoneBlock, loopDoneBlock.FirstInstruction())
	printf(b, "\n")
	b.CreateRetVoid()
}

func addDEBUGTRACELibFRAME(mod llvm.Module, decls *RTDecls, printf func(llvm.Builder, string, ...llvm.Value)) {
	fn := llvm.AddFunction(mod, "DEBUGTRACE.FRAME", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	callerFramePtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	callerFrame := b.CreateLoad(callerFramePtr, "")
	localVarCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	localVarCount := b.CreateLoad(localVarCountPtr, "")
	printf(b, "FRAME %p\n CALLER FRAME %p\n LOCAL VARS[%d]", frameParam, callerFrame, localVarCount)
	localVarArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	localVarArray := b.CreateLoad(localVarArrayPtr, "")
	localVarLoopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(localVarLoopBlock)

	b.SetInsertPoint(localVarLoopBlock, localVarLoopBlock.FirstInstruction())
	localVarIndex := b.CreatePHI(llvm.Int32Type(), "")
	var nextLocalVarIndex llvm.Value
	var localVarLoopBodyBlock llvm.BasicBlock
	defer func() {
		localVarIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextLocalVarIndex,
		}, []llvm.BasicBlock{
			entryBlock,
			localVarLoopBodyBlock,
		})
	}()
	localVarIndexCheck := b.CreateICmp(llvm.IntUGE, localVarIndex, localVarCount, "")
	localVarLoopBodyBlock = llvm.AddBasicBlock(fn, "")
	localVarLoopDoneBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(localVarIndexCheck, localVarLoopDoneBlock, localVarLoopBodyBlock)

	b.SetInsertPoint(localVarLoopBodyBlock, localVarLoopBodyBlock.FirstInstruction())
	localVarNodePtr := b.CreateGEP(localVarArray, []llvm.Value{localVarIndex}, "")
	localVarNode := b.CreateLoad(localVarNodePtr, "")
	printf(b, " %p@%p", localVarNode, localVarNodePtr)
	nextLocalVarIndex = b.CreateAdd(localVarIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(localVarLoopBlock)

	b.SetInsertPoint(localVarLoopDoneBlock, localVarLoopDoneBlock.FirstInstruction())
	iteratorsCountPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	iteratorsCount := b.CreateLoad(iteratorsCountPtr, "")
	printf(b, "\n ITERATORS[%d]", iteratorsCount)
	iteratorsArrayPtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 4, false),
	}, "")
	iteratorsArray := b.CreateLoad(iteratorsArrayPtr, "")
	iteratorsArrayLoopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(iteratorsArrayLoopBlock)

	b.SetInsertPoint(iteratorsArrayLoopBlock, iteratorsArrayLoopBlock.FirstInstruction())
	iteratorsArrayIndex := b.CreatePHI(llvm.Int32Type(), "")
	var nextIteratorsArrayIndex llvm.Value
	var edgeLoopBlock llvm.BasicBlock
	defer func() {
		iteratorsArrayIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIteratorsArrayIndex,
		}, []llvm.BasicBlock{
			localVarLoopDoneBlock,
			edgeLoopBlock,
		})
	}()
	iteratorsArrayIndexCheck := b.CreateICmp(llvm.IntUGE, iteratorsArrayIndex, iteratorsCount, "")
	iteratorsArrayLoopBodyBlock := llvm.AddBasicBlock(fn, "")
	iteratorsArrayLoopDoneBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(iteratorsArrayIndexCheck, iteratorsArrayLoopDoneBlock, iteratorsArrayLoopBodyBlock)

	b.SetInsertPoint(iteratorsArrayLoopBodyBlock, iteratorsArrayLoopBodyBlock.FirstInstruction())
	nextIteratorsArrayIndex = b.CreateAdd(iteratorsArrayIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	edgeCountPtr := b.CreateGEP(iteratorsArray, []llvm.Value{
		iteratorsArrayIndex,
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	edgeCount := b.CreateLoad(edgeCountPtr, "")
	printf(b, "\n  ITERATOR[%d]", edgeCount)
	edgeArrayPtr := b.CreateGEP(iteratorsArray, []llvm.Value{
		iteratorsArrayIndex,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	edgeArray := b.CreateLoad(edgeArrayPtr, "")
	edgeLoopBlock = llvm.AddBasicBlock(fn, "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(edgeLoopBlock, edgeLoopBlock.FirstInstruction())
	edgeIndex := b.CreatePHI(llvm.Int32Type(), "")
	var nextEdgeIndex llvm.Value
	var edgeLoopBodyBlock llvm.BasicBlock
	defer func() {
		edgeIndex.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextEdgeIndex,
		}, []llvm.BasicBlock{
			iteratorsArrayLoopBodyBlock,
			edgeLoopBodyBlock,
		})
	}()
	edgeIndexCheck := b.CreateICmp(llvm.IntUGE, edgeIndex, edgeCount, "")
	edgeLoopBodyBlock = llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(edgeIndexCheck, iteratorsArrayLoopBlock, edgeLoopBodyBlock)

	b.SetInsertPoint(edgeLoopBodyBlock, edgeLoopBodyBlock.FirstInstruction())
	edgePtr := b.CreateGEP(edgeArray, []llvm.Value{edgeIndex}, "")
	edge := b.CreateLoad(edgePtr, "")
	printf(b, " %p", edge)
	nextEdgeIndex = b.CreateAdd(edgeIndex, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(edgeLoopBlock)

	b.SetInsertPoint(iteratorsArrayLoopDoneBlock, iteratorsArrayLoopDoneBlock.FirstInstruction())
	printf(b, "\n")
	b.CreateRetVoid()
}

func addDEBUGTRACELibARGS(mod llvm.Module, decls *RTDecls, printf func(llvm.Builder, string, ...llvm.Value)) {
	fn := llvm.AddFunction(mod, "DEBUGTRACE.ARGS", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
	frameParam := fn.FirstParam()

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	printf(b, "ARGS")
	callerFramePtr := b.CreateGEP(frameParam, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	callerFrame := b.CreateLoad(callerFramePtr, "")
	callerFrameCheck := b.CreateICmp(llvm.IntEQ, callerFrame, llvm.ConstNull(decls.PFrameType), "")
	doneBlock := llvm.AddBasicBlock(fn, "")
	getArgCountBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(callerFrameCheck, doneBlock, getArgCountBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	printf(b, "\n")
	b.CreateRetVoid()

	b.SetInsertPoint(getArgCountBlock, getArgCountBlock.FirstInstruction())
	argCountPtr := b.CreateGEP(callerFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	argCount := b.CreateLoad(argCountPtr, "")
	printf(b, "[%d]", argCount)
	argArrayPtr := b.CreateGEP(callerFrame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	argArray := b.CreateLoad(argArrayPtr, "")
	argLoopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(argLoopBlock)

	b.SetInsertPoint(argLoopBlock, argLoopBlock.FirstInstruction())
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	var printArgBlock llvm.BasicBlock
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
		}, []llvm.BasicBlock{
			getArgCountBlock,
			printArgBlock,
		})
	}()
	indexCheck := b.CreateICmp(llvm.IntUGE, index, argCount, "")
	argLoopBodyBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(indexCheck, doneBlock, argLoopBodyBlock)

	b.SetInsertPoint(argLoopBodyBlock, argLoopBodyBlock.FirstInstruction())
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	argPtr := b.CreateGEP(argArray, []llvm.Value{index}, "")
	arg := b.CreateLoad(argPtr, "")
	argCheck := b.CreateICmp(llvm.IntEQ, arg, llvm.ConstNull(decls.PPNodeType), "")
	loadNodeBlock := llvm.AddBasicBlock(fn, "")
	printArgBlock = llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(argCheck, printArgBlock, loadNodeBlock)

	b.SetInsertPoint(loadNodeBlock, loadNodeBlock.FirstInstruction())
	node1 := b.CreateLoad(arg, "")
	b.CreateBr(printArgBlock)

	b.SetInsertPoint(printArgBlock, printArgBlock.FirstInstruction())
	node := b.CreatePHI(decls.PNodeType, "")
	node.AddIncoming([]llvm.Value{
		llvm.ConstNull(decls.PNodeType),
		node1,
	}, []llvm.BasicBlock{
		argLoopBodyBlock,
		loadNodeBlock,
	})
	printf(b, " %p@%p", node, arg)
	b.CreateBr(argLoopBlock)
}

func addDEBUGTRACELibPAGES(mod llvm.Module, decls *RTDecls, printf func(llvm.Builder, string, ...llvm.Value)) {
	fn := llvm.AddFunction(mod, "DEBUGTRACE.PAGES", llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(fn, "")

	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	compactionEligibleCountPtr := b.CreateGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	compactionEligibleCount := b.CreateLoad(compactionEligibleCountPtr, "")
	printf(b, "PAGES %d\n", compactionEligibleCount)
	initialPage := b.CreateGEP(decls.GlobalState, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	pageLoopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(pageLoopBlock)

	b.SetInsertPoint(pageLoopBlock, pageLoopBlock.FirstInstruction())
	page := b.CreatePHI(decls.PPageType, "")
	var nextPage llvm.Value
	var printPageBlock llvm.BasicBlock
	defer func() {
		page.AddIncoming([]llvm.Value{
			initialPage,
			nextPage,
		}, []llvm.BasicBlock{
			entryBlock,
			printPageBlock,
		})
	}()
	pageCheck := b.CreateICmp(llvm.IntEQ, page, llvm.ConstNull(decls.PPageType), "")
	doneBlock := llvm.AddBasicBlock(fn, "")
	pageLoopBodyBlock := llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(pageCheck, doneBlock, pageLoopBodyBlock)

	b.SetInsertPoint(doneBlock, doneBlock.FirstInstruction())
	b.CreateRetVoid()

	b.SetInsertPoint(pageLoopBodyBlock, pageLoopBodyBlock.FirstInstruction())
	nextPagePtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	nextPage = b.CreateLoad(nextPagePtr, "")
	countLiveNodesLoopBlock := llvm.AddBasicBlock(fn, "")
	b.CreateBr(countLiveNodesLoopBlock)

	b.SetInsertPoint(countLiveNodesLoopBlock, countLiveNodesLoopBlock.FirstInstruction())
	index := b.CreatePHI(llvm.Int32Type(), "")
	var nextIndex llvm.Value
	var countLiveNodeBlock, incrementLiveNodeCountBlock llvm.BasicBlock
	defer func() {
		index.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			nextIndex,
			nextIndex,
		}, []llvm.BasicBlock{
			pageLoopBodyBlock,
			countLiveNodeBlock,
			incrementLiveNodeCountBlock,
		})
	}()
	count := b.CreatePHI(llvm.Int32Type(), "")
	var nextCount llvm.Value
	defer func() {
		count.AddIncoming([]llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			count,
			nextCount,
		}, []llvm.BasicBlock{
			pageLoopBodyBlock,
			countLiveNodeBlock,
			incrementLiveNodeCountBlock,
		})
	}()
	indexCheck := b.CreateICmp(llvm.IntUGE, index, llvm.ConstInt(llvm.Int32Type(), pageSize, false), "")
	countLiveNodeBlock = llvm.AddBasicBlock(fn, "")
	incrementLiveNodeCountBlock = llvm.AddBasicBlock(fn, "")
	printPageBlock = llvm.AddBasicBlock(fn, "")
	b.CreateCondBr(indexCheck, printPageBlock, countLiveNodeBlock)

	b.SetInsertPoint(countLiveNodeBlock, countLiveNodeBlock.FirstInstruction())
	nextIndex = b.CreateAdd(index, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	livePtr := b.CreateGEP(page, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
		index,
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	live := b.CreateLoad(livePtr, "")
	b.CreateCondBr(live, incrementLiveNodeCountBlock, countLiveNodesLoopBlock)

	b.SetInsertPoint(incrementLiveNodeCountBlock, incrementLiveNodeCountBlock.FirstInstruction())
	nextCount = b.CreateAdd(count, llvm.ConstInt(llvm.Int32Type(), 1, false), "")
	b.CreateBr(countLiveNodesLoopBlock)

	b.SetInsertPoint(printPageBlock, printPageBlock.FirstInstruction())
	printf(b, " PAGE %p NEXT %p LIVE NODES %d\n", page, nextPage, count)
	b.CreateBr(pageLoopBlock)
}
