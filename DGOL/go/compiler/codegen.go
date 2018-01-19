package main

import (
	"llvm.org/llvm/bindings/go/llvm"
)

func CodeGen(context llvm.Context, astModule *ASTModule, libs []string) llvm.Module {
	var mod llvm.Module
	var decls *RTDecls
	if astModule.Program == nil {
		mod, decls = RuntimeDecls(context, astModule.Filename)
	} else {
		mod, decls = RuntimeDefs(context, astModule.Filename)
		for _, lib := range libs {
			AddDGOLLib(mod, decls, lib)
		}
	}
	codeGenMetadata(mod)
	dib := llvm.NewDIBuilder(mod)
	defer dib.Destroy()
	defer dib.Finalize()
	diCU := dib.CreateCompileUnit(llvm.DICompileUnit{
		Language:       llvm.DwarfLang(1),
		File:           astModule.Filename,
		Dir:            astModule.Dir,
		Producer:       "DGOLC",
		Optimized:      false,
		Flags:          "",
		RuntimeVersion: 1,
	})
	diFile := dib.CreateFile(astModule.Filename, astModule.Dir)
	diVoidType := dib.CreateBasicType(llvm.DIBasicType{
		Name:       "void",
		SizeInBits: 0,
		Encoding:   0,
	})
	diSubroutineType := dib.CreateSubroutineType(llvm.DISubroutineType{
		File:       diFile,
		Parameters: []llvm.Metadata{diVoidType},
	})
	for _, routine := range astModule.Subroutine {
		diScope := dib.CreateFunction(diCU, llvm.DIFunction{
			Name:         astModule.Name + "." + routine.Name,
			LinkageName:  "." + astModule.Name + "." + routine.Name,
			File:         diFile,
			Line:         routine.LineNumber,
			Type:         diSubroutineType,
			LocalToUnit:  !routine.Exported,
			IsDefinition: true,
			ScopeLine:    routine.LineNumber,
			Flags:        0,
			Optimized:    false,
		})
		rout := codeGenRoutine(mod, decls, diScope, astModule.Name, routine, false)
		if routine.Exported {
			llvm.AddAlias(mod, rout.Type(), rout, astModule.Name+"."+routine.Name)
		}
	}
	if astModule.Program != nil {
		diScope := dib.CreateFunction(diCU, llvm.DIFunction{
			Name:         astModule.Name,
			LinkageName:  "main",
			File:         diFile,
			Line:         astModule.Program.LineNumber,
			Type:         diSubroutineType,
			LocalToUnit:  true,
			IsDefinition: true,
			ScopeLine:    astModule.Program.LineNumber,
			Flags:        0,
			Optimized:    false,
		})
		codeGenRoutine(mod, decls, diScope, astModule.Name, *astModule.Program, true)
	}
	return mod
}

func codeGenRoutine(mod llvm.Module, decls *RTDecls, diScope llvm.Metadata, modName string, routine ASTRoutine, isMain bool) llvm.Value {
	var rout llvm.Value
	var callerFrame llvm.Value
	if isMain {
		rout = llvm.AddFunction(mod, "main", llvm.FunctionType(llvm.VoidType(), []llvm.Type{}, false))
		callerFrame = llvm.ConstNull(decls.PFrameType)
	} else {
		rout = mod.NamedFunction("." + modName + "." + routine.Name)
		if rout.IsNil() {
			rout = llvm.AddFunction(mod, "."+modName+"."+routine.Name, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
		}
		callerFrame = rout.FirstParam()
	}
	rout.SetSubprogram(diScope)

	b := mod.Context().NewBuilder()
	defer b.Dispose()

	entryBlock := llvm.AddBasicBlock(rout, "")
	b.SetInsertPoint(entryBlock, entryBlock.FirstInstruction())
	b.SetCurrentDebugLocation(uint(routine.LineNumber), 0, diScope, llvm.Metadata{})
	frame := b.CreateAlloca(decls.FrameType, "")
	callerFramePtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 0, false),
	}, "")
	b.CreateStore(callerFrame, callerFramePtr)

	varCountPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 1, false),
	}, "")
	b.CreateStore(llvm.ConstInt(llvm.Int32Type(), uint64(len(routine.Vars)), false), varCountPtr)

	var varArray llvm.Value
	if len(routine.Vars) == 0 {
		varArray = llvm.ConstNull(decls.PPPNodeType)
	} else {
		varArray = b.CreateArrayAlloca(decls.PNodeType, llvm.ConstInt(llvm.Int32Type(), uint64(len(routine.Vars)), false), "")
		varArraySizeof := llvm.ConstPtrToInt(llvm.ConstGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(len(routine.Vars)), false)}), llvm.Int32Type())
		varArrayRawPtr := b.CreateBitCast(varArray, llvm.PointerType(llvm.Int8Type(), 0), "")
		b.CreateCall(decls.Memset, []llvm.Value{
			varArrayRawPtr,
			llvm.ConstInt(llvm.Int8Type(), 0, false),
			varArraySizeof,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstInt(llvm.Int1Type(), 0, false),
		}, "")
	}
	varArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 2, false),
	}, "")
	b.CreateStore(varArray, varArrayPtr)

	doEdgesCountPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 3, false),
	}, "")
	b.CreateStore(llvm.ConstInt(llvm.Int32Type(), uint64(routine.DoEdgesCount), false), doEdgesCountPtr)

	var doEdgesArray llvm.Value
	if routine.DoEdgesCount == 0 {
		doEdgesArray = llvm.ConstNull(decls.PDoEdgesIteratorType)
	} else {
		doEdgesArray = b.CreateArrayAlloca(decls.DoEdgesIteratorType, llvm.ConstInt(llvm.Int32Type(), uint64(routine.DoEdgesCount), false), "")
		doEdgesArraySizeof := llvm.ConstPtrToInt(llvm.ConstGEP(llvm.ConstNull(decls.PDoEdgesIteratorType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(routine.DoEdgesCount), false)}), llvm.Int32Type())
		doEdgesArrayRawPtr := b.CreateBitCast(doEdgesArray, llvm.PointerType(llvm.Int8Type(), 0), "")
		b.CreateCall(decls.Memset, []llvm.Value{
			doEdgesArrayRawPtr,
			llvm.ConstInt(llvm.Int8Type(), 0, false),
			doEdgesArraySizeof,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstInt(llvm.Int1Type(), 0, false),
		}, "")
	}
	doEdgesArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 4, false),
	}, "")
	b.CreateStore(doEdgesArray, doEdgesArrayPtr)

	callArgsMaxCountPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 5, false),
	}, "")
	b.CreateStore(llvm.ConstInt(llvm.Int32Type(), uint64(routine.CallArgsMaxCount), false), callArgsMaxCountPtr)

	var callArgsArray llvm.Value
	if routine.CallArgsMaxCount == 0 {
		callArgsArray = llvm.ConstNull(decls.PPPNodeType)
	} else {
		callArgsArray = b.CreateArrayAlloca(decls.PPNodeType, llvm.ConstInt(llvm.Int32Type(), uint64(routine.CallArgsMaxCount), false), "")
		callArgsArraySizeof := llvm.ConstPtrToInt(llvm.ConstGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(routine.CallArgsMaxCount), false)}), llvm.Int32Type())
		callArgsArrayRawPtr := b.CreateBitCast(callArgsArray, llvm.PointerType(llvm.Int8Type(), 0), "")
		b.CreateCall(decls.Memset, []llvm.Value{
			callArgsArrayRawPtr,
			llvm.ConstInt(llvm.Int8Type(), 0, false),
			callArgsArraySizeof,
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstInt(llvm.Int1Type(), 0, false),
		}, "")
	}
	callArgsArrayPtr := b.CreateGEP(frame, []llvm.Value{
		llvm.ConstInt(llvm.Int32Type(), 0, false),
		llvm.ConstInt(llvm.Int32Type(), 6, false),
	}, "")
	b.CreateStore(callArgsArray, callArgsArrayPtr)

	var callerArgCount, callerArgArray llvm.Value
	if routine.ParameterCount == 0 {
		callerArgCount = llvm.ConstInt(llvm.Int32Type(), 0, false)
		callerArgArray = llvm.ConstNull(decls.PPPNodeType)
	} else {
		callerArgCountPtr := b.CreateGEP(callerFrame, []llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstInt(llvm.Int32Type(), 5, false),
		}, "")
		callerArgCount = b.CreateLoad(callerArgCountPtr, "")
		callerArgArrayPtr := b.CreateGEP(callerFrame, []llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), 0, false),
			llvm.ConstInt(llvm.Int32Type(), 6, false),
		}, "")
		callerArgArray = b.CreateLoad(callerArgArrayPtr, "")
	}

	//... llvm.dbg.addr for vars

	epilogueBlock := llvm.AddBasicBlock(rout, "")
	routBodyBlock := llvm.AddBasicBlock(rout, "")
	b.CreateBr(routBodyBlock)

	b.SetInsertPoint(epilogueBlock, epilogueBlock.FirstInstruction())
	b.SetCurrentDebugLocation(uint(routine.EndLineNumber), 0, diScope, llvm.Metadata{})
	for i := 0; i < routine.DoEdgesCount; i++ {
		edgesArrayPtr := b.CreateGEP(doEdgesArray, []llvm.Value{
			llvm.ConstInt(llvm.Int32Type(), uint64(i), false),
			llvm.ConstInt(llvm.Int32Type(), 1, false),
		}, "")
		edgesArray := b.CreateLoad(edgesArrayPtr, "")
		edgesArrayCheck := b.CreateICmp(llvm.IntEQ, edgesArray, llvm.ConstNull(decls.PPNodeType), "")
		freeBlock := llvm.AddBasicBlock(rout, "")
		nextBlock := llvm.AddBasicBlock(rout, "")
		b.CreateCondBr(edgesArrayCheck, nextBlock, freeBlock)

		b.SetInsertPoint(freeBlock, freeBlock.FirstInstruction())
		edgesArrayRawPtr := b.CreateBitCast(edgesArray, llvm.PointerType(llvm.Int8Type(), 0), "")
		b.CreateCall(decls.Free, []llvm.Value{edgesArrayRawPtr}, "")
		b.CreateBr(nextBlock)

		b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
	}
	if !isMain {
		b.CreateCall(decls.Compact, []llvm.Value{callerFrame}, "")
	}
	b.CreateRetVoid()

	getVarPtr := func(astVar ASTVar) llvm.Value {
		if !astVar.IsCallArg {
			return b.CreateGEP(varArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(astVar.Index), false)}, "")
		}
		checkIndexBlock := llvm.AddBasicBlock(rout, "")
		checkCallerArgBlock := llvm.AddBasicBlock(rout, "")
		useLocalBlock := llvm.AddBasicBlock(rout, "")
		returnVarPtrBlock := llvm.AddBasicBlock(rout, "")

		b.CreateBr(checkIndexBlock)

		b.SetInsertPoint(checkIndexBlock, checkIndexBlock.FirstInstruction())
		indexCheck := b.CreateICmp(llvm.IntUGE, llvm.ConstInt(llvm.Int32Type(), uint64(astVar.Index), false), callerArgCount, "")
		b.CreateCondBr(indexCheck, useLocalBlock, checkCallerArgBlock)

		b.SetInsertPoint(checkCallerArgBlock, checkCallerArgBlock.FirstInstruction())
		callerArgPtr := b.CreateGEP(callerArgArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(astVar.Index), false)}, "")
		callerArg := b.CreateLoad(callerArgPtr, "")
		callerArgCheck := b.CreateICmp(llvm.IntEQ, callerArg, llvm.ConstNull(decls.PPNodeType), "")
		b.CreateCondBr(callerArgCheck, useLocalBlock, returnVarPtrBlock)

		b.SetInsertPoint(useLocalBlock, useLocalBlock.FirstInstruction())
		localVarPtr := b.CreateGEP(varArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(astVar.Index), false)}, "")
		b.CreateBr(returnVarPtrBlock)

		b.SetInsertPoint(returnVarPtrBlock, returnVarPtrBlock.FirstInstruction())
		varPtr := b.CreatePHI(decls.PPNodeType, "")
		varPtr.AddIncoming([]llvm.Value{
			callerArg,
			localVarPtr,
		}, []llvm.BasicBlock{
			checkCallerArgBlock,
			useLocalBlock,
		})
		return varPtr
	}
	getVar := func(astVar ASTVar) llvm.Value {
		checkNodeBlock := llvm.AddBasicBlock(rout, "")
		newNodeBlock := llvm.AddBasicBlock(rout, "")
		returnNodeBlock := llvm.AddBasicBlock(rout, "")

		b.CreateBr(checkNodeBlock)

		b.SetInsertPoint(checkNodeBlock, checkNodeBlock.FirstInstruction())
		varPtr := getVarPtr(astVar)
		initialNode := b.CreateLoad(varPtr, "")
		nodeCheck := b.CreateICmp(llvm.IntEQ, initialNode, llvm.ConstNull(decls.PNodeType), "")
		b.CreateCondBr(nodeCheck, newNodeBlock, returnNodeBlock)

		b.SetInsertPoint(newNodeBlock, newNodeBlock.FirstInstruction())
		newNode := b.CreateCall(decls.NewNode, []llvm.Value{frame}, "")
		b.CreateStore(newNode, varPtr)
		b.CreateBr(returnNodeBlock)

		b.SetInsertPoint(returnNodeBlock, returnNodeBlock.FirstInstruction())
		node := b.CreatePHI(decls.PNodeType, "")
		node.AddIncoming([]llvm.Value{
			initialNode,
			newNode,
		}, []llvm.BasicBlock{
			checkNodeBlock,
			newNodeBlock,
		})
		return node
	}
	getVal := func(val *ASTVar) llvm.Value {
		if val == nil {
			return b.CreateCall(decls.NewNode, []llvm.Value{frame}, "")
		}
		return getVar(*val)
	}

	var genStmt func(ASTStatement) bool
	var genIfBranch func(ASTStatement, ASTIfBranch, **llvm.BasicBlock) bool
	doStack := make(map[int]llvm.BasicBlock)
	genStmts := func(stmts []ASTStatement) bool {
		for _, stmt := range stmts {
			if !genStmt(stmt) {
				return false
			}
		}
		return true
	}
	genStmt = func(stmt ASTStatement) bool {
		b.SetCurrentDebugLocation(uint(stmt.LineNumber), 0, diScope, llvm.Metadata{})
		switch stmt.Type {
		case StmtLetEq:
			varPtr := getVarPtr(*stmt.Args[0])
			val := getVal(stmt.Args[1])
			b.CreateStore(val, varPtr)
		case StmtLetAddEdge:
			varNode := getVar(*stmt.Args[0])
			val := getVal(stmt.Args[1])
			b.CreateCall(decls.AddEdge, []llvm.Value{varNode, val}, "")
		case StmtLetRemoveEdge:
			varNode := getVar(*stmt.Args[0])
			val := getVal(stmt.Args[1])
			b.CreateCall(decls.RemoveEdge, []llvm.Value{varNode, val}, "")
		case StmtIf:
			var endifBlock *llvm.BasicBlock = nil
			fellthru := false
			for _, ifBranch := range stmt.IfBranches {
				fellthru = genIfBranch(stmt, ifBranch, &endifBlock)
			}
			if fellthru {
				b.SetCurrentDebugLocation(uint(stmt.EndLineNumber), 0, diScope, llvm.Metadata{})
				if endifBlock == nil {
					block := llvm.AddBasicBlock(rout, "")
					endifBlock = &block
				}
				b.CreateBr(*endifBlock)
				b.SetInsertPoint(*endifBlock, endifBlock.FirstInstruction())
			}
			return endifBlock != nil
		case StmtCall:
			callArgsArraySizeof := llvm.ConstPtrToInt(llvm.ConstGEP(llvm.ConstNull(decls.PPNodeType), []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(routine.CallArgsMaxCount), false)}), llvm.Int32Type())
			callArgsArrayRawPtr := b.CreateBitCast(callArgsArray, llvm.PointerType(llvm.Int8Type(), 0), "")
			b.CreateCall(decls.Memset, []llvm.Value{
				callArgsArrayRawPtr,
				llvm.ConstInt(llvm.Int8Type(), 0, false),
				callArgsArraySizeof,
				llvm.ConstInt(llvm.Int32Type(), 0, false),
				llvm.ConstInt(llvm.Int1Type(), 0, false),
			}, "")
			for i, arg := range stmt.Args {
				if arg == nil {
					continue
				}
				varPtr := getVarPtr(*arg)
				argPtr := b.CreateGEP(callArgsArray, []llvm.Value{llvm.ConstInt(llvm.Int32Type(), uint64(i), false)}, "")
				b.CreateStore(varPtr, argPtr)
			}
			var callTargetName string
			if stmt.CallTargetModule == "" {
				callTargetName = "." + modName + "." + stmt.CallTargetRoutine
			} else {
				callTargetName = stmt.CallTargetModule + "." + stmt.CallTargetRoutine
			}
			callTarget := mod.NamedFunction(callTargetName)
			if callTarget.IsNil() {
				callTarget = llvm.AddFunction(mod, callTargetName, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
			}
			b.CreateCall(callTarget, []llvm.Value{frame}, "")
		case StmtReturn:
			b.CreateBr(epilogueBlock)
			return false
		case StmtDoLoop:
			loopBlock := llvm.AddBasicBlock(rout, "")
			b.CreateBr(loopBlock)

			b.SetInsertPoint(loopBlock, loopBlock.FirstInstruction())
			if hasExit(stmt.DoLoopIndex, stmt) {
				exitBlock := llvm.AddBasicBlock(rout, "")
				doStack[stmt.DoLoopIndex] = exitBlock
				if genStmts(stmt.Statements) {
					b.SetCurrentDebugLocation(uint(stmt.EndLineNumber), 0, diScope, llvm.Metadata{})
					b.CreateBr(loopBlock)
				}

				b.SetInsertPoint(exitBlock, exitBlock.FirstInstruction())
			} else {
				if genStmts(stmt.Statements) {
					b.SetCurrentDebugLocation(uint(stmt.EndLineNumber), 0, diScope, llvm.Metadata{})
					b.CreateBr(loopBlock)
				}
				return false
			}
		case StmtDoEdges:
			checkIteratorArraySizeBlock := llvm.AddBasicBlock(rout, "")
			checkIteratorArrayNullBlock := llvm.AddBasicBlock(rout, "")
			//... more blocks
			//... freeIteratorArrayBlock
			//... allocIteratorArrayBlock
			copyEdgesBlock := llvm.AddBasicBlock(rout, "")
			//... checkLoopIndexBlock
			//... checkLoopEdgeBlock
			//... loopBodyBlock
			enddoBlock := llvm.AddBasicBlock(rout, "")
			exitLoopBlock := llvm.AddBasicBlock(rout, "")
			doStack[stmt.DoLoopIndex] = enddoBlock

			var0Ptr := getVarPtr(*stmt.Args[0])
			node1 := getVar(*stmt.Args[1])
			node1EdgesArraySizePtr := b.CreateGEP(node1, []llvm.Value{
				llvm.ConstInt(llvm.Int32Type(), 0, false),
				llvm.ConstInt(llvm.Int32Type(), 2, false),
			}, "")
			node1EdgesArraySize := b.CreateLoad(node1EdgesArraySizePtr, "")
			node1EdgesArraySizeCheck := b.CreateICmp(llvm.IntEQ, node1EdgesArraySize, llvm.ConstInt(llvm.Int32Type(), 0, false), "")
			b.CreateCondBr(node1EdgesArraySizeCheck, exitLoopBlock, checkIteratorArraySizeBlock)

			b.SetInsertPoint(checkIteratorArraySizeBlock, checkIteratorArraySizeBlock.FirstInstruction())
			iteratorArraySizePtr := b.CreateGEP(doEdgesArray, []llvm.Value{
				llvm.ConstInt(llvm.Int32Type(), uint64(stmt.DoEdgesIndex), false),
				llvm.ConstInt(llvm.Int32Type(), 0, false),
			}, "")
			initialIteratorArraySize := b.CreateLoad(iteratorArraySizePtr, "")
			iteratorArrayPtr := b.CreateGEP(doEdgesArray, []llvm.Value{
				llvm.ConstInt(llvm.Int32Type(), uint64(stmt.DoEdgesIndex), false),
				llvm.ConstInt(llvm.Int32Type(), 1, false),
			}, "")
			initialIteratorArray := b.CreateLoad(iteratorArrayPtr, "")
			initialIteratorArraySizeCheck := b.CreateICmp(llvm.IntUGE, initialIteratorArraySize, node1EdgesArraySize, "")
			b.CreateCondBr(initialIteratorArraySizeCheck, copyEdgesBlock, checkIteratorArrayNullBlock)

			b.SetInsertPoint(checkIteratorArrayNullBlock, checkIteratorArrayNullBlock.FirstInstruction())
			iteratorArrayNullCheck := b.CreateICmp(llvm.IntEQ, initialIteratorArray, llvm.ConstNull(decls.PPNodeType), "")
			_ = iteratorArrayNullCheck
			//... if null, allocIteratorArrayBlock, else freeIteratorArrayBlock
			b.CreateBr(enddoBlock)

			//... freeIteratorArrayBlock
			//... call free
			//... br allocIteratorArrayBlock

			//... allocIteratorArrayBlock
			//... call malloc
			//... store
			//... br copyEdgesBlock

			b.SetInsertPoint(copyEdgesBlock, copyEdgesBlock.FirstInstruction())
			//... iteratorArray = phi initialIteratorArray/checkIteratorArraySizeBlock, newIteratorArray/allocIteratorArrayBlock
			//... iteratorArraySize = phi initialIteratorArraySize/checkIteratorArraySizeBlock, node1EdgesArraySize/allocIteratorArrayBlock
			//... call memset - clear iteratorArray
			//... call memcpy - copy edges
			//... br checkLoopIndexBlock
			b.CreateBr(enddoBlock)

			//... checkLoopIndexBlock
			//... loopIndex = phi
			//... if loopIndex >= node1EdgesArraySize, enddoBlock, else checkLoopEdgeBlock

			//... checkLoopEdgeBlock
			//... loopEdge = load iteratorArray[loopIndex]
			//... if loopEdge == null, checkLoopIndexBlock, else loopBodyBlock

			//... loopBodyBlock
			//... store loopEdge var0Ptr
			//... if genStmts, br checkLoopIndexBlock
			_ = var0Ptr

			b.SetInsertPoint(enddoBlock, enddoBlock.FirstInstruction())
			b.SetCurrentDebugLocation(uint(stmt.EndLineNumber), 0, diScope, llvm.Metadata{})
			//... call memset - clear iteratorArray
			b.CreateBr(exitLoopBlock)

			b.SetInsertPoint(exitLoopBlock, exitLoopBlock.FirstInstruction())
			nextBlock := llvm.AddBasicBlock(rout, "")
			b.CreateBr(nextBlock)

			b.SetInsertPoint(nextBlock, nextBlock.FirstInstruction())
		case StmtExit:
			b.CreateBr(doStack[stmt.DoLoopIndex])
			return false
		}
		return true
	}
	genIfBranch = func(ifStmt ASTStatement, ifBranch ASTIfBranch, endifBlock **llvm.BasicBlock) bool {
		b.SetCurrentDebugLocation(uint(ifBranch.LineNumber), 0, diScope, llvm.Metadata{})
		switch ifBranch.Type {
		case IfBranchEq:
			thenBlock := llvm.AddBasicBlock(rout, "")
			elseBlock := llvm.AddBasicBlock(rout, "")
			arg0 := getVar(ifBranch.Args[0])
			arg1 := getVar(ifBranch.Args[1])
			check := b.CreateICmp(llvm.IntEQ, arg0, arg1, "")
			b.CreateCondBr(check, thenBlock, elseBlock)

			b.SetInsertPoint(thenBlock, thenBlock.FirstInstruction())
			if genStmts(ifBranch.Statements) {
				b.SetCurrentDebugLocation(uint(ifStmt.EndLineNumber), 0, diScope, llvm.Metadata{})
				if *endifBlock == nil {
					block := llvm.AddBasicBlock(rout, "")
					*endifBlock = &block
				}
				b.CreateBr(**endifBlock)
			}

			b.SetInsertPoint(elseBlock, elseBlock.FirstInstruction())
		case IfBranchEdge:
			thenBlock := llvm.AddBasicBlock(rout, "")
			elseBlock := llvm.AddBasicBlock(rout, "")
			arg0 := getVar(ifBranch.Args[0])
			arg1 := getVar(ifBranch.Args[1])
			check := b.CreateCall(decls.HasEdge, []llvm.Value{arg0, arg1}, "")
			b.CreateCondBr(check, thenBlock, elseBlock)

			b.SetInsertPoint(thenBlock, thenBlock.FirstInstruction())
			if genStmts(ifBranch.Statements) {
				b.SetCurrentDebugLocation(uint(ifStmt.EndLineNumber), 0, diScope, llvm.Metadata{})
				if *endifBlock == nil {
					block := llvm.AddBasicBlock(rout, "")
					*endifBlock = &block
				}
				b.CreateBr(**endifBlock)
			}

			b.SetInsertPoint(elseBlock, elseBlock.FirstInstruction())
		case IfBranchElse:
			elseBlock := llvm.AddBasicBlock(rout, "")
			b.CreateBr(elseBlock)

			b.SetInsertPoint(elseBlock, elseBlock.FirstInstruction())
			return genStmts(ifBranch.Statements)
		}
		return true
	}

	b.SetInsertPoint(routBodyBlock, routBodyBlock.FirstInstruction())
	if genStmts(routine.Statements) {
		b.SetCurrentDebugLocation(uint(routine.EndLineNumber), 0, diScope, llvm.Metadata{})
		b.CreateBr(epilogueBlock)
	}

	return rout
}

func hasExit(doLoopIndex int, stmt ASTStatement) bool {
	if stmt.Type == StmtExit && stmt.DoLoopIndex == doLoopIndex {
		return true
	}
	for _, s := range stmt.Statements {
		if hasExit(doLoopIndex, s) {
			return true
		}
	}
	for _, ifBranch := range stmt.IfBranches {
		for _, s := range ifBranch.Statements {
			if hasExit(doLoopIndex, s) {
				return true
			}
		}
	}
	return false
}

func codeGenMetadata(mod llvm.Module) {
	mod.AddNamedMetadataOperand("llvm.module.flags", mod.Context().MDNode([]llvm.Metadata{
		llvm.ConstInt(llvm.Int32Type(), 2, false).ConstantAsMetadata(),
		mod.Context().MDString("Dwarf Version"),
		llvm.ConstInt(llvm.Int32Type(), 4, false).ConstantAsMetadata(),
	}))
	mod.AddNamedMetadataOperand("llvm.module.flags", mod.Context().MDNode([]llvm.Metadata{
		llvm.ConstInt(llvm.Int32Type(), 2, false).ConstantAsMetadata(),
		mod.Context().MDString("Debug Info Version"),
		llvm.ConstInt(llvm.Int32Type(), 3, false).ConstantAsMetadata(),
	}))

	mod.AddNamedMetadataOperand("llvm.ident", mod.Context().MDNode([]llvm.Metadata{
		mod.Context().MDString("DGOLC"),
	}))
}
