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
}
