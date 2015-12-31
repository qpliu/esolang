package main

import (
	"bytes"
	"os/exec"
	"testing"
)

func checkLLC(t *testing.T, ast *Ast) {
	for _, funcDecl := range ast.Funcs {
		WalkStmts(funcDecl, func(stmt Stmt, inLoop bool) error {
			ann := stmt.LLVMAnnotation()
			*ann = LLVMStmtAnnotation{}
			return nil
		})
		WalkExprs(funcDecl, func(stmt Stmt, inLoop bool, expr Expr, inAssign bool) error {
			ann := expr.LLVMAnnotation()
			*ann = LLVMExprAnnotation{}
			return nil
		})
	}
	var buf bytes.Buffer
	llcmd := exec.Command("llc", "-filetype=null")
	llcmd.Stderr = &buf
	out, err := llcmd.StdinPipe()
	if err != nil {
		t.Error(err.Error())
	}
	if err := llcmd.Start(); err != nil {
		t.Error(err.Error())
	}
	if err := LLVMCodeGen(ast, out); err != nil {
		t.Error(err.Error())
	}
	out.Close()
	if err := llcmd.Wait(); err != nil {
		t.Errorf("%s: %s", err.Error(), buf.String())
	}
}

func TestPrologue(t *testing.T) {
	ast, err := testCompile(`
type a { a }
func TestPrologue(p1, p2 a) {
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
	expected := `declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)define void @__copy({i8, [0 x i1]}* %srcval, i8 %srcoffset, {i8, [0 x i1]}* %destval, i8 %destoffset, i8 %bitsize) { br label %l1 l1: %1 = phi i8 [0, %0], [%8, %l2] %2 = icmp ult i8 %1, %bitsize br i1 %2, label %l2, label %l3 l2: %3 = add i8 %1, %srcoffset %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %srcval, i32 0, i32 1, i8 %3 %5 = load i1, i1* %4 %6 = add i8 %1, %destoffset %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %destval, i32 0, i32 1, i8 %6 store i1 %5, i1* %7 %8 = add i8 %1, 1 br label %l1 l3: ret void }define {i8, [0 x i1]}* @__alloc2({i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l3, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l3, label %l2 l2: ret {i8, [0 x i1]}* null ; panic - this should not happen
l3: %6 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1] ret {i8, [0 x i1]}* %6 }define {i8, [0 x i1]}* @__alloc3({i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l4, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l4, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l4, label %l3 l3: ret {i8, [0 x i1]}* null ; panic - this should not happen
l4: %9 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2] ret {i8, [0 x i1]}* %9 }define {i8, [0 x i1]}* @__alloc4({i8, [0 x i1]}* %a0,{i8, [0 x i1]}* %a1,{i8, [0 x i1]}* %a2,{i8, [0 x i1]}* %a3) { l0: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a0, i32 0, i32 0 %1 = load i8, i8* %0 %2 = icmp eq i8 %1, 0 br i1 %2, label %l5, label %l1 l1: %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a1, i32 0, i32 0 %4 = load i8, i8* %3 %5 = icmp eq i8 %4, 0 br i1 %5, label %l5, label %l2 l2: %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a2, i32 0, i32 0 %7 = load i8, i8* %6 %8 = icmp eq i8 %7, 0 br i1 %8, label %l5, label %l3 l3: %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %a3, i32 0, i32 0 %10 = load i8, i8* %9 %11 = icmp eq i8 %10, 0 br i1 %11, label %l5, label %l4 l4: ret {i8, [0 x i1]}* null ; panic - this should not happen
l5: %12 = phi {i8, [0 x i1]}* [%a0, %l0], [%a1, %l1], [%a2, %l2], [%a3, %l3] ret {i8, [0 x i1]}* %12 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenPrologue expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestEmpty(t *testing.T) {
	ast, err := testCompile(`func TestEmpty() {}`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestEmpty"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestEmpty"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestEmpty() { entry: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestSimple(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestSimple() a { var r a; return r}`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestSimple"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestSimple"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define {{i8, [0 x i1]}*, i8} @TestSimple({i8, [0 x i1]}* %retval) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %2, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %4 = load i8, i8* %3 %5 = add i8 %4, 1 store i8 %5, i8* %3 %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %9 = load i8, i8* %8 %10 = sub i8 %9, 1 store i8 %10, i8* %8 call void @__copy({i8, [0 x i1]}* %6, i8 %7, {i8, [0 x i1]}* %retval, i8 0, i8 1) %11 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %retval, 0 ret {{i8, [0 x i1]}*, i8} %11 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestSimple2(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestSimple2(a a) a { return a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestSimple2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestSimple2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define {{i8, [0 x i1]}*, i8} @TestSimple2({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %retval) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 br label %block1 block1: %4 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %5 = select i1 1, i8 %offset0, i8 0 %6 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %7 = load i8, i8* %6 %8 = sub i8 %7, 1 store i8 %8, i8* %6 %9 = icmp eq {i8, [0 x i1]}* %4, %value0 br i1 %9, label %block1.0, label %block1.1 block1.0: %10 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %4, 0 %11 = insertvalue {{i8, [0 x i1]}*, i8} %10, i8 %5, 1 ret {{i8, [0 x i1]}*, i8} %11 block1.1: call void @__copy({i8, [0 x i1]}* %4, i8 %5, {i8, [0 x i1]}* %retval, i8 0, i8 1) %12 = insertvalue {{i8, [0 x i1]}*, i8} {{i8, [0 x i1]}* null, i8 0}, {i8, [0 x i1]}* %retval, 0 ret {{i8, [0 x i1]}*, i8} %12 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestInfiniteLoop(t *testing.T) {
	ast, err := testCompile(`func TestInfiniteLoop() { for {} }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestInfiniteLoop"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestInfiniteLoop"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestInfiniteLoop() { entry: br label %block1 block1: br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestBreak(t *testing.T) {
	ast, err := testCompile(`func TestBreak() { for { break } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestBreak"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestBreak"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestBreak() { entry: br label %block1 block1: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestBreak2(t *testing.T) {
	ast, err := testCompile(`func TestBreak2() { for { for { break } } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestBreak2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestBreak2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestBreak2() { entry: br label %block1 block1: br label %block2 block2: br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestBreak3(t *testing.T) {
	ast, err := testCompile(`func TestBreak3() { for a { for { break a } } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestBreak3"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestBreak3"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestBreak3() { entry: br label %block1 block1: br label %block2 block2: ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestBreak4(t *testing.T) {
	ast, err := testCompile(`func TestBreak4() { for a { for { break a } } for {} }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestBreak4"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestBreak4"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestBreak4() { entry: br label %block1 block1: br label %block2 block2: br label %block3 block3: br label %block3 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestSetClear(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestSetClear() { var a a; set a.a; clear a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestSetClear"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestSetClear"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestSetClear() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %2, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %3 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %4 = load i8, i8* %3 %5 = add i8 %4, 1 store i8 %5, i8* %3 %6 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %7 = select i1 1, i8 %offset0, i8 0 %8 = add i8 %7, 0 %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %6, i32 0, i32 1, i8 %8 store i1 1, i1* %9 %10 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %11 = select i1 1, i8 %offset0, i8 0 %12 = add i8 %11, 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %10, i32 0, i32 1, i8 %12 store i1 0, i1* %13 %14 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %15 = load i8, i8* %14 %16 = sub i8 %15, 1 store i8 %16, i8* %14 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestIf(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestIf(a a) { if a.a { set a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestIf"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestIf"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestIf({i8, [0 x i1]}* %value0,i8 %offset0) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 br label %block1 block1: %4 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %5 = select i1 1, i8 %offset0, i8 0 %6 = add i8 %5, 0 %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %4, i32 0, i32 1, i8 %6 %8 = load i1, i1* %7 br i1 %8, label %block2, label %block1.0 block1.0: %9 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %10 = load i8, i8* %9 %11 = sub i8 %10, 1 store i8 %11, i8* %9 ret void block2: %12 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %13 = select i1 1, i8 %offset0, i8 0 %14 = add i8 %13, 0 %15 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %12, i32 0, i32 1, i8 %14 store i1 1, i1* %15 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %17 = load i8, i8* %16 %18 = sub i8 %17, 1 store i8 %18, i8* %16 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestIf2(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestIf2(a a) { if a.a { set a.a } else { clear a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestIf2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestIf2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestIf2({i8, [0 x i1]}* %value0,i8 %offset0) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 br label %block1 block1: %4 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %5 = select i1 1, i8 %offset0, i8 0 %6 = add i8 %5, 0 %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %4, i32 0, i32 1, i8 %6 %8 = load i1, i1* %7 br i1 %8, label %block2, label %block3 block2: %9 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %10 = select i1 1, i8 %offset0, i8 0 %11 = add i8 %10, 0 %12 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %9, i32 0, i32 1, i8 %11 store i1 1, i1* %12 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %14 = load i8, i8* %13 %15 = sub i8 %14, 1 store i8 %15, i8* %13 ret void block3: %16 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %17 = select i1 1, i8 %offset0, i8 0 %18 = add i8 %17, 0 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %16, i32 0, i32 1, i8 %18 store i1 0, i1* %19 %20 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %21 = load i8, i8* %20 %22 = sub i8 %21, 1 store i8 %22, i8* %20 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestIf3(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestIf3(a, b a) { if a.a { set a.a } else if b.a { clear a.a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestIf3"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestIf3"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestIf3({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %value1,i8 %offset1) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %5 = load i8, i8* %4 %6 = add i8 %5, 1 store i8 %6, i8* %4 br label %block1 block1: %7 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %8 = select i1 1, i8 %offset0, i8 0 %9 = add i8 %8, 0 %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %7, i32 0, i32 1, i8 %9 %11 = load i1, i1* %10 br i1 %11, label %block2, label %block3 block2: %12 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %13 = select i1 1, i8 %offset0, i8 0 %14 = add i8 %13, 0 %15 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %12, i32 0, i32 1, i8 %14 store i1 1, i1* %15 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %17 = load i8, i8* %16 %18 = sub i8 %17, 1 store i8 %18, i8* %16 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %20 = load i8, i8* %19 %21 = sub i8 %20, 1 store i8 %21, i8* %19 ret void block3: %22 = select i1 1, {i8, [0 x i1]}* %value1, {i8, [0 x i1]}* null %23 = select i1 1, i8 %offset1, i8 0 %24 = add i8 %23, 0 %25 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %22, i32 0, i32 1, i8 %24 %26 = load i1, i1* %25 br i1 %26, label %block4, label %block3.0 block3.0: %27 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %28 = load i8, i8* %27 %29 = sub i8 %28, 1 store i8 %29, i8* %27 %30 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %31 = load i8, i8* %30 %32 = sub i8 %31, 1 store i8 %32, i8* %30 ret void block4: %33 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %34 = select i1 1, i8 %offset0, i8 0 %35 = add i8 %34, 0 %36 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %33, i32 0, i32 1, i8 %35 store i1 0, i1* %36 %37 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %38 = load i8, i8* %37 %39 = sub i8 %38, 1 store i8 %39, i8* %37 %40 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %41 = load i8, i8* %40 %42 = sub i8 %41, 1 store i8 %42, i8* %40 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestIf4(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestIf4(a, b a) { if a.a { set a.a } clear a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestIf4"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestIf4"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestIf4({i8, [0 x i1]}* %value0,i8 %offset0,{i8, [0 x i1]}* %value1,i8 %offset1) { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %2 = load i8, i8* %1 %3 = add i8 %2, 1 store i8 %3, i8* %1 %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %5 = load i8, i8* %4 %6 = add i8 %5, 1 store i8 %6, i8* %4 br label %block1 block1: %7 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %8 = select i1 1, i8 %offset0, i8 0 %9 = add i8 %8, 0 %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %7, i32 0, i32 1, i8 %9 %11 = load i1, i1* %10 br i1 %11, label %block2, label %block3 block2: %12 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %13 = select i1 1, i8 %offset0, i8 0 %14 = add i8 %13, 0 %15 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %12, i32 0, i32 1, i8 %14 store i1 1, i1* %15 br label %block3 block3: %16 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %17 = select i1 1, i8 %offset0, i8 0 %18 = add i8 %17, 0 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %16, i32 0, i32 1, i8 %18 store i1 0, i1* %19 %20 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %21 = load i8, i8* %20 %22 = sub i8 %21, 1 store i8 %22, i8* %20 %23 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %24 = load i8, i8* %23 %25 = sub i8 %24, 1 store i8 %25, i8* %23 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestScope(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestScope() { { var a a } var a a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestScope"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestScope"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestScope() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %6 = load i8, i8* %5 %7 = add i8 %6, 1 store i8 %7, i8* %5 %8 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %9 = load i8, i8* %8 %10 = sub i8 %9, 1 store i8 %10, i8* %8 %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 %11 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %12 = load i8, i8* %11 %13 = add i8 %12, 1 store i8 %13, i8* %11 %14 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %15 = load i8, i8* %14 %16 = sub i8 %15, 1 store i8 %16, i8* %14 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestScope2(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestScope2() { for { var a a } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestScope2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestScope2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestScope2() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %2, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %3 = bitcast {i8, [0 x i1]}* %value0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %5 = load i8, i8* %4 %6 = add i8 %5, 1 store i8 %6, i8* %4 %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %8 = load i8, i8* %7 %9 = sub i8 %8, 1 store i8 %9, i8* %7 br label %block1 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestAssign(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestAssign() { var a a; var b a; b = a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestAssign"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestAssign"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestAssign() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %6 = load i8, i8* %5 %7 = add i8 %6, 1 store i8 %7, i8* %5 %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 %8 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %9 = load i8, i8* %8 %10 = add i8 %9, 1 store i8 %10, i8* %8 %11 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %12 = select i1 1, i8 %offset0, i8 0 %value2 = select i1 1, {i8, [0 x i1]}* %11, {i8, [0 x i1]}* null %offset2 = select i1 1, i8 %12, i8 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value2, i32 0, i32 0 %14 = load i8, i8* %13 %15 = add i8 %14, 1 store i8 %15, i8* %13 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %17 = load i8, i8* %16 %18 = sub i8 %17, 1 store i8 %18, i8* %16 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %20 = load i8, i8* %19 %21 = sub i8 %20, 1 store i8 %21, i8* %19 %22 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value2, i32 0, i32 0 %23 = load i8, i8* %22 %24 = sub i8 %23, 1 store i8 %24, i8* %22 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestAssign2(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestAssign2() { var a a; var b a; b.a = a.a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestAssign2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestAssign2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestAssign2() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %6 = load i8, i8* %5 %7 = add i8 %6, 1 store i8 %7, i8* %5 %value1 = select i1 1, {i8, [0 x i1]}* %alloca1, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 0, i8 0 %8 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %9 = load i8, i8* %8 %10 = add i8 %9, 1 store i8 %10, i8* %8 %11 = select i1 1, {i8, [0 x i1]}* %value1, {i8, [0 x i1]}* null %12 = select i1 1, i8 %offset1, i8 0 %13 = add i8 %12, 0 %14 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %15 = select i1 1, i8 %offset0, i8 0 %16 = add i8 %15, 0 call void @__copy({i8, [0 x i1]}* %14, i8 %16, {i8, [0 x i1]}* %11, i8 %13, i8 1) %17 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %18 = load i8, i8* %17 %19 = sub i8 %18, 1 store i8 %19, i8* %17 %20 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %21 = load i8, i8* %20 %22 = sub i8 %21, 1 store i8 %22, i8* %20 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestFuncall(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func TestFuncall() { var a a; a = id(a) }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestFuncall"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestFuncall"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestFuncall() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %6 = load i8, i8* %5 %7 = add i8 %6, 1 store i8 %7, i8* %5 %8 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %9 = select i1 1, i8 %offset0, i8 0 %10 = call {{i8, [0 x i1]}*, i8} @id({i8, [0 x i1]}* %8, i8 %9,{i8, [0 x i1]}* %alloca1) %11 = extractvalue {{i8, [0 x i1]}*, i8} %10, 0 %12 = extractvalue {{i8, [0 x i1]}*, i8} %10, 1 %value1 = select i1 1, {i8, [0 x i1]}* %11, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %12, i8 0 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %14 = load i8, i8* %13 %15 = add i8 %14, 1 store i8 %15, i8* %13 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %17 = load i8, i8* %16 %18 = sub i8 %17, 1 store i8 %18, i8* %16 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %20 = load i8, i8* %19 %21 = sub i8 %20, 1 store i8 %21, i8* %19 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestVarInitializer(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func TestVarInitializer() { var a a; var b a = a }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestVarInitializer"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestVarInitializer"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestVarInitializer() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %3, i8 0, i8 %sizeof.a, i32 0, i1 0) %4 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %5 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %6 = load i8, i8* %5 %7 = add i8 %6, 1 store i8 %7, i8* %5 %8 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %9 = select i1 1, i8 %offset0, i8 0 %value1 = select i1 1, {i8, [0 x i1]}* %8, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %9, i8 0 %10 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %11 = load i8, i8* %10 %12 = add i8 %11, 1 store i8 %12, i8* %10 %13 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %14 = load i8, i8* %13 %15 = sub i8 %14, 1 store i8 %15, i8* %13 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %17 = load i8, i8* %16 %18 = sub i8 %17, 1 store i8 %18, i8* %16 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestVarInitializer2(t *testing.T) {
	ast, err := testCompile(`type a { a } func id(a a) a { return a } func TestVarInitializer2() { var a a; var b a = id(a) }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestVarInitializer2"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestVarInitializer2"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestVarInitializer2() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = alloca i8, i8 %sizeof.a %alloca2 = bitcast i8* %3 to {i8, [0 x i1]}* %4 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) %5 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %5, i8 0, i8 %sizeof.a, i32 0, i1 0) %6 = bitcast {i8, [0 x i1]}* %alloca2 to i8* call void @llvm.memset.p0i8.i8(i8* %6, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %8 = load i8, i8* %7 %9 = add i8 %8, 1 store i8 %9, i8* %7 %10 = select i1 1, {i8, [0 x i1]}* %value0, {i8, [0 x i1]}* null %11 = select i1 1, i8 %offset0, i8 0 %12 = call {{i8, [0 x i1]}*, i8} @id({i8, [0 x i1]}* %10, i8 %11,{i8, [0 x i1]}* %alloca2) %13 = extractvalue {{i8, [0 x i1]}*, i8} %12, 0 %14 = extractvalue {{i8, [0 x i1]}*, i8} %12, 1 %value1 = select i1 1, {i8, [0 x i1]}* %13, {i8, [0 x i1]}* null %offset1 = select i1 1, i8 %14, i8 0 %15 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %16 = load i8, i8* %15 %17 = add i8 %16, 1 store i8 %17, i8* %15 %18 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %19 = load i8, i8* %18 %20 = sub i8 %19, 1 store i8 %20, i8* %18 %21 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %22 = load i8, i8* %21 %23 = sub i8 %22, 1 store i8 %23, i8* %21 ret void }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}

func TestVarInLoop(t *testing.T) {
	ast, err := testCompile(`type a { a } func TestVarInLoop() { var a a; for { var b a; a = b } }`)
	if err != nil {
		t.Errorf("testCompile error: %s", err.Error())
	}

	LLVMCodeGenAnnotateFunc(ast, ast.Funcs["TestVarInLoop"])
	var buf bytes.Buffer
	if err := LLVMCodeGenFunc(ast, ast.Funcs["TestVarInLoop"], &buf); err != nil {
		t.Errorf("LLVMCodeGenFunc error: %s", err.Error())
	}
	expected := `define void @TestVarInLoop() { entry: %0 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* null, i32 0, i32 1, i8 1 %sizeof.a = ptrtoint i1* %0 to i8 %1 = alloca i8, i8 %sizeof.a %alloca0 = bitcast i8* %1 to {i8, [0 x i1]}* %2 = alloca i8, i8 %sizeof.a %alloca1 = bitcast i8* %2 to {i8, [0 x i1]}* %3 = alloca i8, i8 %sizeof.a %alloca2 = bitcast i8* %3 to {i8, [0 x i1]}* %4 = bitcast {i8, [0 x i1]}* %alloca0 to i8* call void @llvm.memset.p0i8.i8(i8* %4, i8 0, i8 %sizeof.a, i32 0, i1 0) %5 = bitcast {i8, [0 x i1]}* %alloca1 to i8* call void @llvm.memset.p0i8.i8(i8* %5, i8 0, i8 %sizeof.a, i32 0, i1 0) %6 = bitcast {i8, [0 x i1]}* %alloca2 to i8* call void @llvm.memset.p0i8.i8(i8* %6, i8 0, i8 %sizeof.a, i32 0, i1 0) br label %block1 block1: %value0 = select i1 1, {i8, [0 x i1]}* %alloca0, {i8, [0 x i1]}* null %offset0 = select i1 1, i8 0, i8 0 %7 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value0, i32 0, i32 0 %8 = load i8, i8* %7 %9 = add i8 %8, 1 store i8 %9, i8* %7 br label %block2 block2: %value1 = phi {i8, [0 x i1]}* [%value0,%block1],[%value3,%block2] %offset1 = phi i8 [%offset0,%block1],[%offset3,%block2] %value2 = call {i8, [0 x i1]}* @__alloc2({i8, [0 x i1]}* %alloca1,{i8, [0 x i1]}* %alloca2) %10 = bitcast {i8, [0 x i1]}* %value2 to i8* call void @llvm.memset.p0i8.i8(i8* %10, i8 0, i8 %sizeof.a, i32 0, i1 0) %offset2 = select i1 1, i8 0, i8 0 %11 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value2, i32 0, i32 0 %12 = load i8, i8* %11 %13 = add i8 %12, 1 store i8 %13, i8* %11 %14 = select i1 1, {i8, [0 x i1]}* %value2, {i8, [0 x i1]}* null %15 = select i1 1, i8 %offset2, i8 0 %value3 = select i1 1, {i8, [0 x i1]}* %14, {i8, [0 x i1]}* null %offset3 = select i1 1, i8 %15, i8 0 %16 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value3, i32 0, i32 0 %17 = load i8, i8* %16 %18 = add i8 %17, 1 store i8 %18, i8* %16 %19 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value1, i32 0, i32 0 %20 = load i8, i8* %19 %21 = sub i8 %20, 1 store i8 %21, i8* %19 %22 = getelementptr {i8, [0 x i1]}, {i8, [0 x i1]}* %value2, i32 0, i32 0 %23 = load i8, i8* %22 %24 = sub i8 %23, 1 store i8 %24, i8* %22 br label %block2 }`
	if buf.String() != expected {
		t.Errorf("LLVMCodeGenFunc expected %s, got %s", expected, buf.String())
	}
	checkLLC(t, ast)
}
