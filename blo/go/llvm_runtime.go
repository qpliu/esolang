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
				ast.LLVMDeclares["malloc"] = "declare i8* void @malloc(i32)"
				ast.LLVMDeclares["llvm.memset.p0i8.i32"] = "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)"
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
		} else if funcDecl.Name == "popStack" && len(funcDecl.Params) == 1 && funcDecl.Params[0].Type.Imported && funcDecl.Params[0].Type.Name == "stack" && funcDecl.Type != nil && !funcDecl.Type.Imported {
			funcDecl.RuntimeLLVM = runtimeLLVMEmitPopStack
		} else if funcDecl.Name == "isEmpty" && len(funcDecl.Params) == 1 && funcDecl.Params[0].Type.Imported && funcDecl.Params[0].Type.Name == "stack" && funcDecl.Type != nil && !funcDecl.Type.Imported {
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
	if _, err := fmt.Fprintf(w, " %%%d = bitcast i8* %s to {i32, i32, i32, [0 x i8]*}* %%%d = getelementptr {i32, i32, i32, [0 x i8]*}, {i32, i32, i32, [0 x i8]*} %%%d, i32 0, i32 0 %%%d = load i32, i32* %%%d %%%d = add i32 %%%d, 1 store i32 %%%d, i32* %%%d", *ssaTemp, imp, *ssaTemp+1, *ssaTemp, *ssaTemp+2, *ssaTemp+1, *ssaTemp+3, *ssaTemp+2, *ssaTemp+3, *ssaTemp+1); err != nil {
		return err
	}
	*ssaTemp += 4
	return nil
}

func runtimeLLVMUnrefStack(imp string, ssaTemp *int, w io.Writer) error {
	if _, err := fmt.Fprintf(w, " call void @__unrefStack(i8* %s) ", imp); err != nil {
		return err
	}
	return nil
}

func runtimeLLVMEmitPushStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	//...
	return nil
}

func runtimeLLVMEmitPopStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	//...
	return nil
}

func runtimeLLVMEmitIsEmptyStack(ast *Ast, funcDecl *Func, w io.Writer) error {
	//...
	return nil
}
