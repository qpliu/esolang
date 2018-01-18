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
	dib.CreateCompileUnit(llvm.DICompileUnit{
		Language:       llvm.DwarfLang(1),
		File:           astModule.Filename,
		Dir:            astModule.Dir,
		Producer:       "DGOLC",
		Optimized:      false,
		Flags:          "",
		RuntimeVersion: 1,
	})
	diFile := dib.CreateFile(astModule.Filename, astModule.Dir)
	for _, routine := range astModule.Subroutine {
		rout := codeGenRoutine(mod, decls, diFile, astModule.Name, routine, false)
		if routine.Exported {
			llvm.AddAlias(mod, rout.Type(), rout, astModule.Name+"."+routine.Name)
		}
	}
	if astModule.Program != nil {
		codeGenRoutine(mod, decls, diFile, astModule.Name, *astModule.Program, true)
	}
	return mod
}

func codeGenRoutine(mod llvm.Module, decls *RTDecls, diFile llvm.Metadata, modName string, routine ASTRoutine, isMain bool) llvm.Value {
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
	b.SetCurrentDebugLocation(uint(routine.LineNumber), 0, diFile, llvm.Metadata{})
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
	b.SetCurrentDebugLocation(uint(routine.EndLineNumber), 0, diFile, llvm.Metadata{})
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
	//... check and free doEdgesArray[..].edgeArray
	if !isMain {
		b.CreateCall(decls.Compact, []llvm.Value{callerFrame}, "")
	}
	b.CreateRetVoid()

	b.SetInsertPoint(routBodyBlock, routBodyBlock.FirstInstruction())
	//...
	b.CreateBr(epilogueBlock)
	_, _ = callerArgCount, callerArgArray

	return rout
}
