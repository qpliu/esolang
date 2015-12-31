package main

import (
	"bytes"
	"testing"
)

func TestLLVMRuntime(t *testing.T) {
	ast, err := testCompile(`
import func putByte(b byte)
type byte { 1, 2, 4, 8, 10, 20, 40, 80 }
`)
	if err != nil {
		t.Errorf("compile error: %s", err.Error())
	}
	if err := AnnotateRuntimeLLVM(ast); err != nil {
		t.Errorf("AnnotateRuntimeLLVM error: %s", err.Error())
	}

	funcDecl := ast.Funcs["putByte"]
	var buf bytes.Buffer
	if err := funcDecl.RuntimeLLVM(ast, funcDecl, &buf); err != nil {
		t.Errorf("RuntimeLLVM error: %s", err.Error())
	}
	expected := `define void @putByte({i8, [0 x i1]}* %a1, i8 %a2) { %1 = alloca i8, i32 1 %2 = zext i1 0 to i8 %3 = add i8 %a2, 0 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 %6 = zext i1 %5 to i8 %7 = shl i8 %6, 0 %8 = or i8 %2, %7 %9 = add i8 %a2, 1 %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %9 %11 = load i1, i1* %10 %12 = zext i1 %11 to i8 %13 = shl i8 %12, 1 %14 = or i8 %8, %13 %15 = add i8 %a2, 2 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %15 %17 = load i1, i1* %16 %18 = zext i1 %17 to i8 %19 = shl i8 %18, 2 %20 = or i8 %14, %19 %21 = add i8 %a2, 3 %22 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %21 %23 = load i1, i1* %22 %24 = zext i1 %23 to i8 %25 = shl i8 %24, 3 %26 = or i8 %20, %25 %27 = add i8 %a2, 4 %28 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %27 %29 = load i1, i1* %28 %30 = zext i1 %29 to i8 %31 = shl i8 %30, 4 %32 = or i8 %26, %31 %33 = add i8 %a2, 5 %34 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %33 %35 = load i1, i1* %34 %36 = zext i1 %35 to i8 %37 = shl i8 %36, 5 %38 = or i8 %32, %37 %39 = add i8 %a2, 6 %40 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %39 %41 = load i1, i1* %40 %42 = zext i1 %41 to i8 %43 = shl i8 %42, 6 %44 = or i8 %38, %43 %45 = add i8 %a2, 7 %46 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 %45 %47 = load i1, i1* %46 %48 = zext i1 %47 to i8 %49 = shl i8 %48, 7 %50 = or i8 %44, %49 store i8 %50, i8* %1 call i32 @write(i32 1, i8* %1, i32 1) ret void }`
	if buf.String() != expected {
		t.Errorf("RuntimeLLVM putByte expected %s, got %s", expected, buf.String())
	}

	ast, err = testCompile(`
import func getByte(b byte+flag)
type byte+flag { 1, 2, 4, 8, 10, 20, 40, 80, EOF }
`)
	if err != nil {
		t.Errorf("compile error: %s", err.Error())
	}
	if err := AnnotateRuntimeLLVM(ast); err != nil {
		t.Errorf("AnnotateRuntimeLLVM error: %s", err.Error())
	}

	funcDecl = ast.Funcs["getByte"]
	buf.Reset()
	if err := funcDecl.RuntimeLLVM(ast, funcDecl, &buf); err != nil {
		t.Errorf("RuntimeLLVM error: %s", err.Error())
	}
	expected = `define void @getByte({i8, [0 x i1]}* %a1, i8 %a2) { %1 = alloca i8, i32 1 %2 = call i32 @read(i32 0,i8* %1,i32 1) %3 = icmp eq i32 1, %2 br i1 %3, label %l2, label %l1 l1: %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 8 store i1 1, i1* %4 ret void l2: %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 8 store i1 0, i1* %5 %6 = load i8, i8* %1 %7 = trunc i8 %6 to i1 %8 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 0 store i1 %7, i1* %8 %9 = lshr i8 %6, 1 %10 = trunc i8 %9 to i1 %11 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 1 store i1 %10, i1* %11 %12 = lshr i8 %9, 1 %13 = trunc i8 %12 to i1 %14 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 2 store i1 %13, i1* %14 %15 = lshr i8 %12, 1 %16 = trunc i8 %15 to i1 %17 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 3 store i1 %16, i1* %17 %18 = lshr i8 %15, 1 %19 = trunc i8 %18 to i1 %20 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 4 store i1 %19, i1* %20 %21 = lshr i8 %18, 1 %22 = trunc i8 %21 to i1 %23 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 5 store i1 %22, i1* %23 %24 = lshr i8 %21, 1 %25 = trunc i8 %24 to i1 %26 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 6 store i1 %25, i1* %26 %27 = lshr i8 %24, 1 %28 = trunc i8 %27 to i1 %29 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 1, i8 7 store i1 %28, i1* %29 %30 = lshr i8 %27, 1 ret void }`
	if buf.String() != expected {
		t.Errorf("RuntimeLLVM getByte expected %s, got %s", expected, buf.String())
	}
}
