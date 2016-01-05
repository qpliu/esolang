package main

// This is written for LLVM 3.7.0

import (
	"errors"
	"fmt"
	"io"
)

func AnnotateRuntimeLLVM(ast *Ast) error {
	ast.LLVMDeclares = make(map[string]string)
	for _, typeDecl := range ast.Types {
		if typeDecl.Imported {
			if typeDecl.Name == "stack" {
				typeDecl.LLVMRTType.WritePrologue = runtimeLLVMPrologueStack
				typeDecl.LLVMRTType.WriteInit = runtimeLLVMInitStack
				typeDecl.LLVMRTType.WriteRef = runtimeLLVMRefStack
				typeDecl.LLVMRTType.WriteUnref = runtimeLLVMUnrefStack
				ast.LLVMDeclares["free"] = "declare void @free(i8*)"
				ast.LLVMDeclares["malloc"] = "declare i8* @malloc(i32)"
				ast.LLVMDeclares["llvm.memset.p0i8.i32"] = "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)"
				ast.LLVMDeclares["llvm.memcpy.p0i8.p0i8.i32"] = "declare void @llvm.memcpy.p0i8.p0i8.i32(i8*,i8*,i32,i32,i1)"
			} else {
				return errors.New(typeDecl.Location.String() + ": Unrecognized import type: " + typeDecl.Name)
			}
		}
	}
	for _, funcDecl := range ast.Funcs {
		if !funcDecl.Imported {
			continue
		}
		if funcDecl.Name == "getByte" && len(funcDecl.Params) == 1 && funcDecl.Type == nil {
			ast.LLVMDeclares["read"] = "declare i32 @read(i32,i8*,i32)"
			funcDecl.RuntimeLLVM = runtimeLLVMEmitGetByte
		} else if funcDecl.Name == "putByte" && len(funcDecl.Params) == 1 && funcDecl.Type == nil {
			ast.LLVMDeclares["write"] = "declare i32 @write(i32,i8*,i32)"
			funcDecl.RuntimeLLVM = runtimeLLVMEmitPutByte
		} else if funcDecl.Name == "pushStack" && len(funcDecl.Params) == 2 && funcDecl.Params[0].Type.Imported && funcDecl.Params[0].Type.Name == "stack" && funcDecl.Type == nil {
			funcDecl.RuntimeLLVM = runtimeLLVMEmitPushStack
		} else if funcDecl.Name == "popStack" && len(funcDecl.Params) == 1 && funcDecl.Params[0].Type.Imported && funcDecl.Params[0].Type.Name == "stack" && funcDecl.Type != nil && importedCount(funcDecl.Type) == 0 {
			funcDecl.RuntimeLLVM = runtimeLLVMEmitPopStack
		} else if funcDecl.Name == "isEmptyStack" && len(funcDecl.Params) == 1 && funcDecl.Params[0].Type.Imported && funcDecl.Params[0].Type.Name == "stack" && funcDecl.Type != nil && importedCount(funcDecl.Type) == 0 {
			funcDecl.RuntimeLLVM = runtimeLLVMEmitIsEmptyStack
		} else {
			return errors.New(funcDecl.Location.String() + ": Unrecognized import function: " + funcDecl.Name)
		}
	}
	return nil
}

func runtimeLLVMEmitGetByte(ast *Ast, funcDecl *Func, w io.Writer) error {
	if _, err := fmt.Fprintf(w, "define void @%s({%s, [0 x i1]}* %%a1, %s %%a2) { %%1 = alloca i8, i32 1 %%2 = call i32 @read(i32 0,i8* %%1,i32 1) %%3 = icmp eq i32 1, %%2 br i1 %%3, label %%l2, label %%l1 l1:", LLVMCanonicalName(funcDecl.Name), LLVMRefcountType(ast), LLVMOffsetType(ast)); err != nil {
		return err
	}
	i := 4
	if funcDecl.Params[0].Type.BitSize() >= 9 {
		if _, err := fmt.Fprintf(w, " %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a1, i32 0, i32 1, %s 8 store i1 1, i1* %%%d ret void l2: %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a1, i32 0, i32 1, %s 8 store i1 0, i1* %%%d", i, LLVMRefcountType(ast), LLVMRefcountType(ast), LLVMOffsetType(ast), i, i+1, LLVMRefcountType(ast), LLVMRefcountType(ast), LLVMOffsetType(ast), i+1); err != nil {
			return err
		}
		i += 2
	} else {
		if _, err := io.WriteString(w, " ret void l2:"); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, " %%%d = load i8, i8* %%1", i); err != nil {
		return err
	}
	for j := 0; j < 8 && j < funcDecl.Params[0].Type.BitSize(); j++ {
		if _, err := fmt.Fprintf(w, " %%%d = trunc i8 %%%d to i1 %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a1, i32 0, i32 1, %s %d store i1 %%%d, i1* %%%d %%%d = lshr i8 %%%d, 1", i+1, i, i+2, LLVMRefcountType(ast), LLVMRefcountType(ast), LLVMOffsetType(ast), j, i+1, i+2, i+3, i); err != nil {
			return err
		}
		i += 3
	}
	if _, err := io.WriteString(w, " ret void }"); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMEmitPutByte(ast *Ast, funcDecl *Func, w io.Writer) error {
	if _, err := fmt.Fprintf(w, "define void @%s({%s, [0 x i1]}* %%a1, %s %%a2) { %%1 = alloca i8, i32 1 %%2 = zext i1 0 to i8", LLVMCanonicalName(funcDecl.Name), LLVMRefcountType(ast), LLVMOffsetType(ast)); err != nil {
		return err
	}
	i := 2
	for j := 0; j < 8 && j < funcDecl.Params[0].Type.BitSize(); j++ {
		if _, err := fmt.Fprintf(w, " %%%d = add %s %%a2, %d %%%d = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%a1, i32 0, i32 1, %s %%%d %%%d = load i1, i1* %%%d %%%d = zext i1 %%%d to i8 %%%d = shl i8 %%%d, %d %%%d = or i8 %%%d, %%%d", i+1, LLVMOffsetType(ast), j, i+2, LLVMRefcountType(ast), LLVMRefcountType(ast), LLVMOffsetType(ast), i+1, i+3, i+2, i+4, i+3, i+5, i+4, j, i+6, i, i+5); err != nil {
			return err
		}
		i += 6
	}
	if _, err := fmt.Fprintf(w, " store i8 %%%d, i8* %%1 call i32 @write(i32 1, i8* %%1, i32 1) ret void }", i); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMPrologueStack(w io.Writer) error {
	// {i32, i32, i32, [0 x i8]*} = reference count, size, capacity, bits
	if _, err := io.WriteString(w, "define void @__unrefStack(i8* %imp) { %1 = bitcast i8* %imp to {i32, i32, i32, [0 x i8]*}* %2 = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*}* %1, i32 0, i32 0 %3 = load i32, i32* %2 %4 = sub i32 %3, 1 store i32 %4, i32* %2 %5 = icmp ne i32 %4, 0 br i1 %5, label %l1, label %l2 l1: ret void l2: call void @free(i8* %imp) ret void }"); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMInitStack(ssaTemp *int, w io.Writer) (int, error) {
	if _, err := fmt.Fprintf(w, " %%%d = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*}* null, i32 1 %%%d = ptrtoint {i32, i32, i32, [0 x i8]*}* %%%d to i32 %%%d = call i8* @malloc(i32 %%%d) call void @llvm.memset.p0i8.i32(i8* %%%d, i8 0, i32 %%%d, i32 0, i1 0)", *ssaTemp, *ssaTemp+1, *ssaTemp, *ssaTemp+2, *ssaTemp+1, *ssaTemp+2, *ssaTemp+1); err != nil {
		return 0, err
	}
	*ssaTemp += 3
	return *ssaTemp - 1, nil
}

func runtimeLLVMRefStack(imp string, ssaTemp *int, w io.Writer) error {
	if _, err := fmt.Fprintf(w, " %%%d = bitcast i8* %s to {i32, i32, i32, [0 x i8]*}* %%%d = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*}* %%%d, i32 0, i32 0 %%%d = load i32, i32* %%%d %%%d = add i32 %%%d, 1 store i32 %%%d, i32* %%%d", *ssaTemp, imp, *ssaTemp+1, *ssaTemp, *ssaTemp+2, *ssaTemp+1, *ssaTemp+3, *ssaTemp+2, *ssaTemp+3, *ssaTemp+1); err != nil {
		return err
	}
	*ssaTemp += 4
	return nil
}

func runtimeLLVMUnrefStack(imp string, ssaTemp *int, w io.Writer) error {
	if _, err := fmt.Fprintf(w, " call void @__unrefStack(i8* %s)", imp); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMEmitPushStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	importCount := importedCount(funcDecl.Params[0].Type)
	if _, err := fmt.Fprintf(w, "define void @%s({%s, [0 x i1]}* %%stackval, %s %%stackoffset, [%d x i8*] %%stackimport, {%s, [0 x i1]}* %%value, %s %%offset) {", LLVMCanonicalName(funcDecl.Name), refCountType, offsetType, importCount, refCountType, offsetType); err != nil {
		return err
	}
	if funcDecl.Params[1].Type.BitSize() > 0 {
		// %1 = bit to push
		if _, err := fmt.Fprintf(w, " entry: %%0 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%value, i32 0, i32 1, %s %%offset %%1 = load i1, i1* %%0", refCountType, refCountType, offsetType); err != nil {
			return err
		}
		// %4 = &stack size %5 = stack size %6 = &stack capacity %7 = stack capacity %8 = &&stack data %9 = &stack data
		if _, err := fmt.Fprintf(w, " %%2 = extractvalue [%d x i8*] %%stackimport, 0 %%3 = bitcast i8* %%2 to {i32,i32,i32,[0 x i8]*}* %%4 = getelementptr {i32,i32,i32,[0 x i8]*}, {i32,i32,i32,[0 x i8]*}* %%3, i32 0, i32 1 %%5 = load i32, i32* %%4 %%6 = getelementptr {i32,i32,i32,[0 x i8]*}, {i32,i32,i32,[0 x i8]*}* %%3, i32 0, i32 2 %%7 = load i32, i32* %%6 %%8 = getelementptr {i32,i32,i32,[0 x i8]*}, {i32,i32,i32,[0 x i8]*}* %%3, i32 0, i32 3 %%9 = load [0 x i8]*, [0 x i8]** %%8", importCount); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, " %%10 = icmp ult i32 %%5, %%7 br i1 %%10, label %%l1, label %%l2 l1: %%11 = phi [0 x i8]* [%%9, %%entry], [%%27, %%l2], [%%27, %%l3] %%12 = add i32 %%5, 1 store i32 %%12, i32* %%4 %%13 = udiv i32 %%5, 8 %%14 = urem i32 %%5, 8 %%15 = getelementptr [0 x i8], [0 x i8]* %%11, i32 0, i32 %%13 %%16 = load i8, i8* %%15 %%17 = trunc i32 %%14 to i8 %%18 = shl i8 1, %%17 %%19 = xor i8 %%18, 255 %%20 = and i8 %%19, %%16 %%21 = zext i1 %%1 to i8 %%22 = shl i8 %%21, %%17 %%23 = or i8 %%20, %%22 store i8 %%23, i8* %%15 ret void"); err != nil {
			return err
		}
		// %26 = (i8*) &new stack data, %27 = &new stack data
		if _, err := fmt.Fprintf(w, " l2: %%24 = add i32 %%7, 128 store i32 %%24, i32* %%6 %%25 = udiv i32 %%24, 8 %%26 = call i8* @malloc(i32 %%25) %%27 = bitcast i8* %%26 to [0 x i8]* store [0 x i8]* %%27, [0 x i8]** %%8 %%28 = icmp eq i32 %%7, 0 br i1 %%28, label %%l1, label %%l3"); err != nil {
			return err
		}
		if _, err := fmt.Fprintf(w, " l3: %%29 = bitcast [0 x i8]* %%9 to i8* %%30 = udiv i32 %%7, 8 call void @llvm.memcpy.p0i8.p0i8.i32(i8* %%26, i8* %%29, i32 %%30, i32 0, i1 0) call void @free(i8* %%29) br label %%l1"); err != nil {
			return err
		}
	} else {
		if _, err := fmt.Fprintf(w, " ret void"); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, " }"); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMEmitPopStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	importCount := importedCount(funcDecl.Params[0].Type)
	if _, err := fmt.Fprintf(w, "define {{%s, [0 x i1]}*, %s} @%s({%s, [0 x i1]}* %%stackval, %s %%stackoffset, [%d x i8*] %%stackimport, {%s, [0 x i1]}* %%retvalue) {", refCountType, offsetType, LLVMCanonicalName(funcDecl.Name), refCountType, offsetType, importCount, refCountType); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, " %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%retvalue, i32 0, i32 0 %%2 = load %s, %s* %%1 %%3 = add %s %%2, 1 store %s %%3, %s* %%1 %%4 = insertvalue {{%s, [0 x i1]}*, %s} {{%s, [0 x i1]}* null, %s 0}, {%s, [0 x i1]}* %%retvalue, 0", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, offsetType, refCountType, offsetType, refCountType); err != nil {
		return err
	}
	// %7 = &stack size, %8 = stack size
	if _, err := fmt.Fprintf(w, " %%5 = extractvalue [%d x i8*] %%stackimport, 0 %%6 = bitcast i8* %%5 to {i32, i32, i32, [0 x i8]*}* %%7 = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*}* %%6, i32 0, i32 0 %%8 = load i32, i32* %%7 %%9 = icmp ugt i32 %%8, 0 br i1 %%9, label %%l1, label %%l2", importCount); err != nil {
		return err
	}
	// %19 = popped bit
	if _, err := fmt.Fprintf(w, " l1: %%10 = sub i32 %%8, 1 store i32 %%10, i32* %%7 %%11 = udiv i32 %%8, 8 %%12 = urem i32 %%8, 8 %%13 = getelementptr {i32,i32,i32,[0 x i8]*}, {i32,i32,i32,[0 x i8]*}* %%6, i32 0, i32 3 %%14 = load [0 x i8]*, [0 x i8]** %%13 %%15 = getelementptr [0 x i8], [0 x i8]* %%14, i32 0, i32 %%11 %%16 = load i8, i8* %%15 %%17 = zext i8 %%16 to i32 %%18 = lshr i32 %%17, %%12 %%19 = trunc i32 %%18 to i1"); err != nil {
		return err
	}
	if funcDecl.Type.BitSize() > 0 {
		if _, err := fmt.Fprintf(w, " %%20 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%retvalue, i32 0, i32 1, %s 0 store i1 %%19, i1* %%20", refCountType, refCountType, offsetType); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, " br label %%l2 l2: ret {{%s, [0 x i1]}*, %s} %%4 }", refCountType, offsetType); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMEmitIsEmptyStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	refCountType := LLVMRefcountType(ast)
	offsetType := LLVMOffsetType(ast)
	importCount := importedCount(funcDecl.Params[0].Type)
	if _, err := fmt.Fprintf(w, "define {{%s, [0 x i1]}*, %s} @%s({%s, [0 x i1]}* %%stackval, %s %%stackoffset, [%d x i8*] %%stackimport, {%s, [0 x i1]}* %%retvalue) {", refCountType, offsetType, LLVMCanonicalName(funcDecl.Name), refCountType, offsetType, importCount, refCountType); err != nil {
		return err
	}
	if _, err := fmt.Fprintf(w, " %%1 = getelementptr {%s, [0 x i1]}, {%s, [0 x i1]}* %%retvalue, i32 0, i32 0 %%2 = load %s, %s* %%1 %%3 = add %s %%2, 1 store %s %%3, %s* %%1 %%4 = insertvalue {{%s, [0 x i1]}*, %s} {{%s, [0 x i1]}* null, %s 0}, {%s, [0 x i1]}* %%retvalue, 0", refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, refCountType, offsetType, refCountType, offsetType, refCountType); err != nil {
		return err
	}
	if funcDecl.Type.BitSize() > 0 {
		if _, err := fmt.Fprintf(w, " %%5 = extractvalue [%d x i8*] %%stackimport, 0 %%6 = bitcast i8* %%5 to {i32, i32, i32, [0 x i8]*}* %%7 = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*}* %%6, i32 0, i32 0 %%8 = load i32, i32* %%7 %%9 = icmp eq i32 %%8, 0 %%10 = getelementptr {%s, [0 x i1]},  {%s, [0 x i1]}* %%retvalue, i32 0, i32 1, %s 0 store i1 %%9, i1* %%10", importCount, refCountType, refCountType, offsetType); err != nil {
			return err
		}
	}
	if _, err := fmt.Fprintf(w, " ret {{%s, [0 x i1]}*, %s} %%4 }", refCountType, offsetType); err != nil {
		return err
	}
	return nil
}
