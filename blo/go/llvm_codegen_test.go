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

	if err := AnnotateRuntimeLLVM(ast); err != nil {
		t.Errorf("AnnotateRuntimeLLVM error: %s", err.Error())
	}
	var buf bytes.Buffer
	if err := LLVMCodeGenPrologue(ast, &buf); err != nil {
		t.Errorf("LLVMCodeGenPrologue error: %s", err.Error())
	}
	expected := `define void @__clear({i8, [0 x i1]}* %v, i8 %bitsize) { br label %l0 l0: %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 store i8 0, i8* %1 br label %l1 l1: %2 = phi i8 [0, %l0], [%5, %l2] %3 = icmp ult i8 %2, %bitsize br i1 %3, label %l2, label %l3 l2: %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 1, i8 %2 store i1 0, i1* %4 %5 = add i8 %2, 1 br label %l1 l3: ret void }define void @__ref({i8, [0 x i1]}* %v) { %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 ret void }define void @__unref({i8, [0 x i1]}* %v) { %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 %2 = load i8, i8* %1 %3 = sub i8 %2, 1 store i8 %3, i8* %1 ret void }define void @__copy({i8, [0 x i1]}* %srcval, i8 %srcoffset, {i8, [0 x i1]}* %destval, i8 %destoffset, i8 %bitsize) { br label %l1 l1: %1 = phi i8 [0, %0], [%8, %l2] %2 = icmp ult i8 %1, %bitsize br i1 %2, label %l2, label %l3 l2: %3 = add i8 %1, %srcoffset %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %srcval, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 %6 = add i8 %1, %destoffset %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %destval, i32 0, i32 1, i8 %6 store i1 %5, i1* %7 %8 = add i8 %1, 1 br label %l1 l3: ret void }define {i8, [0 x i1]}* @__alloc2(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l3, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l3, label %l2 l2: ret {i8, [0 x i1]}* null ; panic - this should not happen
l3: %6 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1] call void @__clear({i8, [0 x i1]}* %6, i8 %bitsize) ret {i8, [0 x i1]}* %6 }define {i8, [0 x i1]}* @__alloc3(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l4, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l4, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l4, label %l3 l3: ret {i8, [0 x i1]}* null ; panic - this should not happen
l4: %9 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2] call void @__clear({i8, [0 x i1]}* %9, i8 %bitsize) ret {i8, [0 x i1]}* %9 }define {i8, [0 x i1]}* @__alloc4(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2,{i8, [0 x i1]}* %a3) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l5, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l5, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l5, label %l3 l3: %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a3, i32 0, i32 0 %10 = load i8, i8* %9 %11 = icmp eq i8 %10, 0 br i1 %11, label %l5, label %l4 l4: ret {i8, [0 x i1]}* null ; panic - this should not happen
l5: %12 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2], [%a3, %l3] call void @__clear({i8, [0 x i1]}* %12, i8 %bitsize) ret {i8, [0 x i1]}* %12 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenPrologue expected %s, got %s", expected, buf.String())
	}
}

func TestEmpty(t *testing.T) {
	ast, err := testCompile(`func a() {}`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestSimple(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() a { var r a; return r}`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define {{i8, [0 x i1]}*, i8} @a({i8, [0 x i1]}* %retval) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %2 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %3 = select i1 1, i8 %offset0, i8 0 call void @__unref({i8, [0 x i1]}* %value0) call void @__copy({i8, [0 x i1]}* %2, i8 %3, {i8, [0 x i1]}* %retval, i8 0, i8 1) %4 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %retval, 0 ret {{i8, [0 x i1]}*, i8} %4 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestSimple2(t *testing.T) {
	ast, err := testCompile(`type a { a } func a(a a) a { return a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define {{i8, [0 x i1]}*, i8} @a({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %retval) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 call void @__ref({i8, [0 x i1]}* %value0) br label %block1 block1: %1 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %2 = select i1 1, i8 %offset0, i8 0 call void @__unref({i8, [0 x i1]}* %value0) %3 = icmp eq {i8, [0 x i1]}* %1, %value0 br i1 %3, label %block1.0, label %block1.1 block1.0: %4 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %1, 0 %5 = insertvalue {{i8, [0 x i1]}*, i8} %4, i8 %2, 1 ret {{i8, [0 x i1]}*, i8} %5 block1.1: call void @__copy({i8, [0 x i1]}* %1, i8 %2, {i8, [0 x i1]}* %retval, i8 0, i8 1) %6 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %retval, 0 ret {{i8, [0 x i1]}*, i8} %6 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestInfiniteLoop(t *testing.T) {
	ast, err := testCompile(`func a() { for {} }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: br label %block1 block1: br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestBreak(t *testing.T) {
	ast, err := testCompile(`func a() { for { break } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: br label %block1 block1: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestBreak2(t *testing.T) {
	ast, err := testCompile(`func a() { for { for { break } } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: br label %block1 block1: br label %block2 block2: br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestBreak3(t *testing.T) {
	ast, err := testCompile(`func a() { for a { for { break a } } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: br label %block1 block1: br label %block2 block2: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestBreak4(t *testing.T) {
	ast, err := testCompile(`func a() { for a { for { break a } } for {} }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: br label %block1 block1: br label %block2 block2: br label %block3 block3: br label %block3 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestSetClear(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { var a a; set a.a; clear a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %2 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %3 = select i1 1, i8 %offset0, i8 0 %4 = add i8 %3, 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %2, i32 0, i32 1, i8 %4 store i1 1, i1* %5 %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 0, i1* %9 call void @__unref({i8, [0 x i1]}* %value0) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestIf(t *testing.T) {
	ast, err := testCompile(`type a { a } func a(a a) { if a.a { set a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a({i8, [0 x i1]}* %value0,i8 %offset0) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 call void @__ref({i8, [0 x i1]}* %value0) br label %block1 block1: %1 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %2 = select i1 1, i8 %offset0, i8 0 %3 = add i8 %2, 0 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %1, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 br i1 %5, label %block2, label %block1.0 block1.0: call void @__unref({i8, [0 x i1]}* %value0) ret void block2: %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 1, i1* %9 call void @__unref({i8, [0 x i1]}* %value0) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestIf2(t *testing.T) {
	ast, err := testCompile(`type a { a } func a(a a) { if a.a { set a.a } else { clear a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a({i8, [0 x i1]}* %value0,i8 %offset0) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 call void @__ref({i8, [0 x i1]}* %value0) br label %block1 block1: %1 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %2 = select i1 1, i8 %offset0, i8 0 %3 = add i8 %2, 0 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %1, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 br i1 %5, label %block2, label %block3 block2: %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 1, i1* %9 call void @__unref({i8, [0 x i1]}* %value0) ret void block3: %10 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %11 = select i1 1, i8 %offset0, i8 0 %12 = add i8 %11, 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %10, i32 0, i32 1, i8 %12 store i1 0, i1* %13 call void @__unref({i8, [0 x i1]}* %value0) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestIf3(t *testing.T) {
	ast, err := testCompile(`type a { a } func a(a, b a) { if a.a { set a.a } else if b.a { clear a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %value1,i8 %offset1) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 call void @__ref({i8, [0 x i1]}* %value0) call void @__ref({i8, [0 x i1]}* %value1) br label %block1 block1: %1 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %2 = select i1 1, i8 %offset0, i8 0 %3 = add i8 %2, 0 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %1, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 br i1 %5, label %block2, label %block3 block2: %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 1, i1* %9 call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void block3: %10 = select i1 1, {i8, [0 x i1]}* %value1, {i8, [0 x i1]}* null %11 = select i1 1, i8 %offset1, i8 0 %12 = add i8 %11, 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %10, i32 0, i32 1, i8 %12 %14 = load i1, i1* %13 br i1 %14, label %block4, label %block3.0 block3.0: call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void block4: %15 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %16 = select i1 1, i8 %offset0, i8 0 %17 = add i8 %16, 0 %18 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %15, i32 0, i32 1, i8 %17 store i1 0, i1* %18 call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestIf4(t *testing.T) {
	ast, err := testCompile(`type a { a } func a(a, b a) { if a.a { set a.a } clear a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %value1,i8 %offset1) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 call void @__ref({i8, [0 x i1]}* %value0) call void @__ref({i8, [0 x i1]}* %value1) br label %block1 block1: %1 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %2 = select i1 1, i8 %offset0, i8 0 %3 = add i8 %2, 0 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %1, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 br i1 %5, label %block2, label %block3 block2: %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 1, i1* %9 br label %block3 block3: %10 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %11 = select i1 1, i8 %offset0, i8 0 %12 = add i8 %11, 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %10, i32 0, i32 1, i8 %12 store i1 0, i1* %13 call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestScope(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { { var a a } var a a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value0) %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value1) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestScope2(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { for { var a a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__clear({i8, [0 x i1]}* %value0, i8 1) call void @__ref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value0) br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestAssign(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { var a a; var b a; b = a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value1) %3 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %4 = select i1 1, i8 %offset0, i8 0 call void @__unref({i8, [0 x i1]}* %value1) %value2 = select i1 1, {i8, [0 x i1]}* %3, {i8, [0 x i1]}* null %offset2 = select i1 1, i8 %4, i8 0 call void @__ref({i8, [0 x i1]}* %value2) call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value2) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestAssign2(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { var a a; var b a; b.a = a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value1) %3 = select i1 1, {i8, [0 x i1]}* %value1, {i8, [0 x i1]}* null %4 = select i1 1, i8 %offset1, i8 0 %5 = add i8 %4, 0 %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 call void @__copy({i8, [0 x i1]}* %6, i8 %8, {i8, [0 x i1]}* %3, i8 %5, i8 1) call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestFuncall(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func a() { var a a; a = id(a) }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %3 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %4 = select i1 1, i8 %offset0, i8 0 %5 = call {{i8, [0 x i1]}*, i8} @id({i8, [0 x i1]}* %3, i8 %4,{i8, [0 x i1]}* %alloca1) %6 = extractvalue {{i8, [0 x i1]}*, i8} %5, 0 %7 = extractvalue {{i8, [0 x i1]}*, i8} %5, 1 call void @__unref({i8, [0 x i1]}* %value0) %value1 = select i1 1, {i8, [0 x i1]}* %6, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %7, i8 0 call void @__ref({i8, [0 x i1]}* %value1) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestVarInitializer(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func a() { var a a; var b a = a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %3 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %4 = select i1 1, i8 %offset0, i8 0 %value1 = select i1 1, {i8, [0 x i1]}* %3, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %4, i8 0 call void @__ref({i8, [0 x i1]}* %value1) call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestVarInitializer2(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func a() { var a a; var b a = id(a) }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = alloca i8, i8 %sizeof.a %alloca2 = bitcast i8* %3 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) call void @__clear({i8, [0 x i1]}* %alloca2, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) %4 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %5 = select i1 1, i8 %offset0, i8 0 %6 = call {{i8, [0 x i1]}*, i8} @id({i8, [0 x i1]}* %4, i8 %5,{i8, [0 x i1]}* %alloca2) %7 = extractvalue {{i8, [0 x i1]}*, i8} %6, 0 %8 = extractvalue {{i8, [0 x i1]}*, i8} %6, 1 %value1 = select i1 1, {i8, [0 x i1]}* %7, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %8, i8 0 call void @__ref({i8, [0 x i1]}* %value1) call void @__unref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}

func TestVarInLoop(t *testing.T) {
	ast, err := testCompile(`type a { a } func a() { var a a; for { var b a; a = b } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["a"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["a"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = alloca i8, i8 %sizeof.a %alloca2 = bitcast i8* %3 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) call void @__clear({i8, [0 x i1]}* %alloca2, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) br label %block2 block2: %value1 = phi {i8, [0 x i1]}* [%value0,%block1],[%value3,%block2] %offset1 = phi i8 [%offset0,%block1],[%offset3,%block2] %value2 = call {i8, [0 x i1]}* @__alloc2({i8, [0 x i1]}* %alloca1,{i8, [0 x i1]}* %alloca2) %offset2 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value2) %4 = select i1 1, {i8, [0 x i1]}* %value2, {i8, [0 x i1]}* null %5 = select i1 1, i8 %offset2, i8 0 call void @__unref({i8, [0 x i1]}* %value1) %value3 = select i1 1, {i8, [0 x i1]}* %4, {i8, [0 x i1]}* null %offset3 = select i1 1, i8 %5, i8 0 call void @__ref({i8, [0 x i1]}* %value3) call void @__unref({i8, [0 x i1]}* %value2) br label %block2 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}
