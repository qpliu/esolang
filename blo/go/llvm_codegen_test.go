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
	expected := `define void @__clear({i8, [0 x i1]}* %v, i8 %bitsize) { 0: %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 store i8 0, i8* %1 br label %2 2: %3 = phi i8 [0, %0], [%7, %5] %4 = icmp lt i8 %3, %bitsize br i1 %4, label %5, label %8 5: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 1, i8 %3 store i1 0, i1* %6 %7 = add i8, %3, 1 br label %2 8: ret void }define void @__ref({i8, [0 x i1]}* %v) { %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 %1 = load i8* %0 %2 = add i8 %1, 1 store i8 %2, i8* %0 ret void }define void @__unref({i8, [0 x i1]}* %v) { %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %v, i32 0, i32 0 %1 = load i8* %0 %2 = sub i8 %1, 1 store i8 %2, i8* %0 ret void }define void @__copy({i8, [0 x i1]}* %srcval, i8 %srcoffset, {i8, [0 x i1]}* %destval, i8 %destoffset, i8 %bitsize) { 0: br label %1 1: %2 = phi i8 [0, %0], [%10, %4] %3 = icmp lt i8 %2, %bitsize br i1 %3, label %4, label %11 4: %5 = add i8 %2, %srcoffset %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %srcval, i32 0, i32 1, i8 %5 %7 = load i1* %6 %8 = add i8 %2, %destoffset %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %destval, i32 0, i32 1, i8 %8 store i1 %7, i1* %9 %10 = add i8 %2, 1 br label %1 11: ret void }define {i8, [0 x i1]}* @__alloc2(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l3, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l3, label %l2 l2: ret {i8, [0 x i1]}* null ; panic - this should not happen
l3: %6 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1] call void @__clear({i8, [0 x i1]}* %6, %bitsize) ret {i8, [0 x i1]}* %6 }define {i8, [0 x i1]}* @__alloc3(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l4, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l4, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l4, label %l3 l3: ret {i8, [0 x i1]}* null ; panic - this should not happen
l4: %9 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2] call void @__clear({i8, [0 x i1]}* %9, %bitsize) ret {i8, [0 x i1]}* %9 }define {i8, [0 x i1]}* @__alloc4(i8 %bitsize,{i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2,{i8, [0 x i1]}* %a3) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l5, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l5, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l5, label %l3 l3: %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a3, i32 0, i32 0 %10 = load i8, i8* %9 %11 = icmp eq i8 %10, 0 br i1 %11, label %l5, label %l4 l4: ret {i8, [0 x i1]}* null ; panic - this should not happen
l5: %12 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2], [%a3, %l3] call void @__clear({i8, [0 x i1]}* %12, %bitsize) ret {i8, [0 x i1]}* %12 }`
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
	expected := `define {{i8, [0 x i1]}*, i8} @a({i8, [0 x i1]}* %rv) { %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 ... %a0.r = alloca/bitcast... call void @__clear({i8, [0 x i1]}* %a0.r, i8 1) call void @__ref({i8, [0 x i1]}* %a0.r) call void @__copy({i8, [0 x i1]}* %a0.r, i8 0, {i8, [0 x i1]}* %rv, i8 0, i8 1) call void @__unref({i8, [0 x i1]}* %a0.r) ret {{i8, [0 x i1]}* %rv, i8 0} }`
	t.SkipNow()
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
	expected := `define {{i8, [0 x i1]}*, i8} @a({i8, [0 x i1]}* %pv.a,i8 %po.a,{i8, [0 x i1]}* %rv) { %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 ret {{i8, [0 x i1]}* %pv.a, i8 %po.a} }`
	t.SkipNow()
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
	expected := ``
	t.SkipNow()
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
	expected := ``
	t.SkipNow()
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
	expected := ``
	t.SkipNow()
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
	expected := ``
	t.SkipNow()
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
	expected := ``
	t.SkipNow()
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
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* 1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* 2 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) call void @__clear({i8, [0 x i1]}* %alloca1, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value0) %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 call void @__ref({i8, [0 x i1]}* %value1) call void @__unref({i8, [0 x i1]}* %value1) ret void }`
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
	expected := `define void @a() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* 1 to {i8, [0 x i1]}* call void @__clear({i8, [0 x i1]}* %alloca0, i8 1) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 call void @__clear({i8, [0 x i1]}* %value0) call void @__ref({i8, [0 x i1]}* %value0) call void @__unref({i8, [0 x i1]}* %value0) br label %block1 }`
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
	expected := ""
	t.SkipNow()
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
	expected := ""
	t.SkipNow()
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
}
