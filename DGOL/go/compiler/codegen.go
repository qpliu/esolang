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
		rout = llvm.AddFunction(mod, "."+modName+"."+routine.Name, llvm.FunctionType(llvm.VoidType(), []llvm.Type{decls.PFrameType}, false))
		callerFrame = rout.FirstParam()
	}

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
		//...
		return llvm.ConstNull(decls.PPNodeType)
	}
	getVar := func(astVar ASTVar) llvm.Value {
		varPtr := getVarPtr(astVar)
		//...
		return b.CreateLoad(varPtr, "")
	}
	getVal := func(val *ASTVar) llvm.Value {
		if val == nil {
			return b.CreateCall(decls.NewNode, []llvm.Value{frame}, "")
		}
		return getVar(*val)
	}

	var genStmt func(ASTStatement) bool
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
			{
				varPtr := getVarPtr(*stmt.Args[0])
				val := getVal(stmt.Args[1])
				b.CreateStore(val, varPtr)
			}
		case StmtLetAddEdge:
			{
				varNode := getVar(*stmt.Args[0])
				val := getVal(stmt.Args[1])
				b.CreateCall(decls.AddEdge, []llvm.Value{varNode, val}, "")
			}
		case StmtLetRemoveEdge:
			{
				varNode := getVar(*stmt.Args[0])
				val := getVal(stmt.Args[1])
				b.CreateCall(decls.RemoveEdge, []llvm.Value{varNode, val}, "")
			}
		case StmtIf:
			//...
		case StmtCall:
			//...
		case StmtReturn:
			b.CreateBr(epilogueBlock)
			return false
		case StmtDoLoop:
			//...
		case StmtDoEdges:
			//...
		case StmtExit:
			//...
		}
		return true
	}

	b.SetInsertPoint(routBodyBlock, routBodyBlock.FirstInstruction())
	if genStmts(routine.Statements) {
		b.CreateBr(epilogueBlock)
	}
	//...
	_, _ = callerArgCount, callerArgArray

	return rout
}
