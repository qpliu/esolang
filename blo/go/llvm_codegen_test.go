package main

import (
	"bytes"
	"testing"
)

func TestPrologue(t *testing.T) {
	ast, err := testCompile(`
type a { a }
func a(p1, p2 a) {
    var v1 a
    {
        var v2 a
    }
    for {
        if v1.a {
            var v2 a
        } else {
            var v2 a
            break
        }
    }
}
`)
	if err != nil {
		t.Errorf("compile error: %s", err.Error())
	}
	checkAnnotatedMax(t, "prologue", ast, 0, 0, 4)

	var buf bytes.Buffer
	if err := LLVMCodeGenPrologue(ast, &buf); err != nil {
		t.Errorf("LLVMCodeGenPrologue error: %s", err.Error())
	}
	expected := `declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)define {i8, [0 x i1]}* @__alloc2(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l3, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l3, label %l2 l2: ret {i8, [0 x i1]}* null ; panic - this should not happen
l3: %6 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1] %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 0 %8 = bitcast i1* %7 to i8* %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 0 %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 %bitsize %11 = ptrtoint i1* %9 to i8 %12 = ptrtoint i1* %10 to i8 %13 = sub i8 %12, %11 call void @llvm.memset.p0i8.i8(i8* %8,i8 0, i8 %13, i32 0, i1 0) ret {i8, [0 x i1]}* %6 }define {i8, [0 x i1]}* @__alloc3(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l4, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l4, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l4, label %l3 l3: ret {i8, [0 x i1]}* null ; panic - this should not happen
l4: %9 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2] %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %9, i32 0, i32 1, i8 0 %11 = bitcast i1* %10 to i8* %12 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 %bitsize %14 = ptrtoint i1* %12 to i8 %15 = ptrtoint i1* %13 to i8 %16 = sub i8 %15, %14 call void @llvm.memset.p0i8.i8(i8* %11,i8 0, i8 %16, i32 0, i1 0) ret {i8, [0 x i1]}* %9 }define {i8, [0 x i1]}* @__alloc4(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2,{i8, [0 x i1]}* %a3) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l5, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l5, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l5, label %l3 l3: %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a3, i32 0, i32 0 %10 = load i8, i8* %9 %11 = icmp eq i8 %10, 0 br i1 %11, label %l5, label %l4 l4: ret {i8, [0 x i1]}* null ; panic - this should not happen
l5: %12 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2], [%a3, %l3] %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %12, i32 0, i32 1, i8 0 %14 = bitcast i1* %13 to i8* %15 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 0 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 %bitsize %17 = ptrtoint i1* %15 to i8 %18 = ptrtoint i1* %16 to i8 %19 = sub i8 %18, %17 call void @llvm.memset.p0i8.i8(i8* %14,i8 0, i8 %19, i32 0, i1 0) ret {i8, [0 x i1]}* %12 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenPrologue expected %s, got %s", expected, buf.String())
	}
}
