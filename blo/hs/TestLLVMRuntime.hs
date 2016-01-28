module TestLLVMRuntime
    (tests)
where

import Test.HUnit(Assertion,Test(..))

import TestLLVM
    (testCodeGen,prologue,memsetDecl,copyDefn,copyrttDefn,alloc2Defn)

tests :: Test
tests = TestList [
    TestCase testVar,
    TestCase testAssign,
    TestCase testReturn,
    TestCase testFuncall,
    TestCase testFuncall2,
    TestCase testVarInLoop,
    TestCase testFuncallInLoop,
    TestCase testStack,
    TestCase testStackFuncs,
    TestCase testStackFuncs2
    ]

testVar :: Assertion
testVar =
    testCodeGen "testVar" "import type test{}func TestVar(){var a test}"
        (prologue ++
            "define void @_TestVar() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = extractvalue {i8*,i8**} %5,0" ++
            " %7 = bitcast i8* %6 to {i8,[0 x i1]}*" ++
            " %8 = select i1 1,i8 0,i8 0" ++
            " %9 = extractvalue {i8*,i8**} %5,1" ++
            " %10 = select i1 1,i8* null,i8* null;test new\n" ++
            " %11 = getelementptr i8*,i8** %9,i8 0" ++
            " store i8* %10,i8** %11" ++
            " %12 = select i1 1,i8 0,i8 0" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = add i8 1,%14" ++
            " store i8 %15,i8* %13" ++
            " %16 = add i8 0,%12" ++
            " %17 = getelementptr i8*,i8** %9,i8 %16" ++
            " %18 = load i8*,i8** %17;test addref\n" ++
            " %19 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %20 = load i8,i8* %19" ++
            " %21 = sub i8 %20,1" ++
            " store i8 %21,i8* %19" ++
            " %22 = add i8 0,%12" ++
            " %23 = getelementptr i8*,i8** %9,i8 %22" ++
            " %24 = load i8*,i8** %23;test unref\n" ++
            " ret void" ++
            " }")

testAssign :: Assertion
testAssign =
    testCodeGen "testAssign" "import type test{}func TestAssign(a test){var b test=a}"
        (prologue ++
            "define void @_TestAssign({i8,[0 x i1]}* %value0,i8 %offset0,i8** %import0,i8 %importoffset0) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %7 = select i1 1,i8 %offset0,i8 0" ++
            " %8 = select i1 1,i8** %import0,i8** null" ++
            " %9 = select i1 1,i8 %importoffset0,i8 0" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %11 = load i8,i8* %10" ++
            " %12 = add i8 1,%11" ++
            " store i8 %12,i8* %10" ++
            " %13 = add i8 0,%9" ++
            " %14 = getelementptr i8*,i8** %8,i8 %13" ++
            " %15 = load i8*,i8** %14;test addref\n" ++
            " %16 = add i8 0,%9" ++
            " %17 = getelementptr i8*,i8** %8,i8 %16" ++
            " %18 = load i8*,i8** %17;test addref\n" ++
            " %19 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %20 = load i8,i8* %19" ++
            " %21 = add i8 1,%20" ++
            " store i8 %21,i8* %19" ++
            " %22 = add i8 0,%9" ++
            " %23 = getelementptr i8*,i8** %8,i8 %22" ++
            " %24 = load i8*,i8** %23;test addref\n" ++
            " %25 = add i8 0,%9" ++
            " %26 = getelementptr i8*,i8** %8,i8 %25" ++
            " %27 = load i8*,i8** %26;test unref\n" ++
            " %28 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %29 = load i8,i8* %28" ++
            " %30 = sub i8 %29,1" ++
            " store i8 %30,i8* %28" ++
            " %31 = add i8 0,%9" ++
            " %32 = getelementptr i8*,i8** %8,i8 %31" ++
            " %33 = load i8*,i8** %32;test unref\n" ++
            " %34 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %35 = load i8,i8* %34" ++
            " %36 = sub i8 %35,1" ++
            " store i8 %36,i8* %34" ++
            " %37 = add i8 0,%9" ++
            " %38 = getelementptr i8*,i8** %8,i8 %37" ++
            " %39 = load i8*,i8** %38;test unref\n" ++
            " ret void" ++
            " }")

testFuncall :: Assertion
testFuncall =
    testCodeGen "testFuncall" "import type test{}func testFuncall(a test){testFuncall(a)}"
        (prologue ++
            "define void @_testFuncall({i8,[0 x i1]}* %value0,i8 %offset0,i8** %import0,i8 %importoffset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = select i1 1,i8** %import0,i8** null" ++
            " %3 = select i1 1,i8 %importoffset0,i8 0" ++
            " %4 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %5 = load i8,i8* %4" ++
            " %6 = add i8 1,%5" ++
            " store i8 %6,i8* %4" ++
            " %7 = add i8 0,%3" ++
            " %8 = getelementptr i8*,i8** %2,i8 %7" ++
            " %9 = load i8*,i8** %8;test addref\n" ++
            " %10 = add i8 0,%3" ++
            " %11 = getelementptr i8*,i8** %2,i8 %10" ++
            " %12 = load i8*,i8** %11;test addref\n" ++
            " call void @_testFuncall({i8,[0 x i1]}* %0,i8 %1,i8** %2,i8 %3)" ++
            " %13 = add i8 0,%3" ++
            " %14 = getelementptr i8*,i8** %2,i8 %13" ++
            " %15 = load i8*,i8** %14;test unref\n" ++
            " %16 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %17 = load i8,i8* %16" ++
            " %18 = sub i8 %17,1" ++
            " store i8 %18,i8* %16" ++
            " %19 = add i8 0,%3" ++
            " %20 = getelementptr i8*,i8** %2,i8 %19" ++
            " %21 = load i8*,i8** %20;test unref\n" ++
            " ret void" ++
            " }")

testFuncall2 :: Assertion
testFuncall2 =
    testCodeGen "testFuncall2" "import type test{}func testFuncall2()test{testFuncall2();var a test;return a}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8,i8**,i8} @_testFuncall2({i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " %6 = alloca i8,i8 %1" ++
            " %7 = alloca i8*,i8 1" ++
            " %8 = insertvalue {i8*,i8**} undef,i8* %6,0" ++
            " %9 = insertvalue {i8*,i8**} %8,i8** %7,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %6,i8 0,i8 %1,i32 0,i1 0)" ++
            " %10 = call {{i8,[0 x i1]}*,i8,i8**,i8} @_testFuncall2({i8*,i8**} %5)" ++
            " %11 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %10,0" ++
            " %12 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %10,1" ++
            " %13 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %10,2" ++
            " %14 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %10,3" ++
            " %15 = add i8 0,%14" ++
            " %16 = getelementptr i8*,i8** %13,i8 %15" ++
            " %17 = load i8*,i8** %16;test unref\n" ++
            " %18 = extractvalue {i8*,i8**} %9,0" ++
            " %19 = bitcast i8* %18 to {i8,[0 x i1]}*" ++
            " %20 = select i1 1,i8 0,i8 0" ++
            " %21 = extractvalue {i8*,i8**} %9,1" ++
            " %22 = select i1 1,i8* null,i8* null;test new\n" ++
            " %23 = getelementptr i8*,i8** %21,i8 0" ++
            " store i8* %22,i8** %23" ++
            " %24 = select i1 1,i8 0,i8 0" ++
            " %25 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %19,i32 0,i32 0" ++
            " %26 = load i8,i8* %25" ++
            " %27 = add i8 1,%26" ++
            " store i8 %27,i8* %25" ++
            " %28 = add i8 0,%24" ++
            " %29 = getelementptr i8*,i8** %21,i8 %28" ++
            " %30 = load i8*,i8** %29;test addref\n" ++
            " %31 = add i8 0,%24" ++
            " %32 = getelementptr i8*,i8** %21,i8 %31" ++
            " %33 = load i8*,i8** %32;test addref\n" ++
            " %34 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %19,i32 0,i32 0" ++
            " %35 = load i8,i8* %34" ++
            " %36 = sub i8 %35,1" ++
            " store i8 %36,i8* %34" ++
            " %37 = add i8 0,%24" ++
            " %38 = getelementptr i8*,i8** %21,i8 %37" ++
            " %39 = load i8*,i8** %38;test unref\n" ++
            " %40 = extractvalue {i8*,i8**} %retval,0" ++
            " %41 = bitcast i8* %40 to {i8,[0 x i1]}*" ++
            " %42 = extractvalue {i8*,i8**} %retval,1" ++
            " call void @copy({i8,[0 x i1]}* %19,i8 %20,{i8,[0 x i1]}* %41,i8 0,i8 0)" ++
            " call void @copyrtt(i8** %21,i8 %24,i8** %42,i8 0,i8 1)" ++
            " %43 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} undef,{i8,[0 x i1]}* %41,0" ++
            " %44 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %43,i8 0,1" ++
            " %45 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %44,i8** %21,2" ++
            " %46 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %45,i8 0,3" ++
            " ret {{i8,[0 x i1]}*,i8,i8**,i8} %46" ++
            " }")

testReturn :: Assertion
testReturn =
    testCodeGen "testReturn" "import type test{}func testReturn()test{var a test;return a}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8,i8**,i8} @_testReturn({i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = extractvalue {i8*,i8**} %5,0" ++
            " %7 = bitcast i8* %6 to {i8,[0 x i1]}*" ++
            " %8 = select i1 1,i8 0,i8 0" ++
            " %9 = extractvalue {i8*,i8**} %5,1" ++
            " %10 = select i1 1,i8* null,i8* null;test new\n" ++
            " %11 = getelementptr i8*,i8** %9,i8 0" ++
            " store i8* %10,i8** %11" ++
            " %12 = select i1 1,i8 0,i8 0" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = add i8 1,%14" ++
            " store i8 %15,i8* %13" ++
            " %16 = add i8 0,%12" ++
            " %17 = getelementptr i8*,i8** %9,i8 %16" ++
            " %18 = load i8*,i8** %17;test addref\n" ++
            " %19 = add i8 0,%12" ++
            " %20 = getelementptr i8*,i8** %9,i8 %19" ++
            " %21 = load i8*,i8** %20;test addref\n" ++
            " %22 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %23 = load i8,i8* %22" ++
            " %24 = sub i8 %23,1" ++
            " store i8 %24,i8* %22" ++
            " %25 = add i8 0,%12" ++
            " %26 = getelementptr i8*,i8** %9,i8 %25" ++
            " %27 = load i8*,i8** %26;test unref\n" ++
            " %28 = extractvalue {i8*,i8**} %retval,0" ++
            " %29 = bitcast i8* %28 to {i8,[0 x i1]}*" ++
            " %30 = extractvalue {i8*,i8**} %retval,1" ++
            " call void @copy({i8,[0 x i1]}* %7,i8 %8,{i8,[0 x i1]}* %29,i8 0,i8 0)" ++
            " call void @copyrtt(i8** %9,i8 %12,i8** %30,i8 0,i8 1)" ++
            " %31 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} undef,{i8,[0 x i1]}* %29,0" ++
            " %32 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %31,i8 0,1" ++
            " %33 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %32,i8** %9,2" ++
            " %34 = insertvalue {{i8,[0 x i1]}*,i8,i8**,i8} %33,i8 0,3" ++
            " ret {{i8,[0 x i1]}*,i8,i8**,i8} %34" ++
            " }")

testVarInLoop :: Assertion
testVarInLoop =
    testCodeGen "testVarInLoop" "import type test{}func testVarInLoop(){var a test;for{}}"
        (prologue ++
            "define void @_testVarInLoop() {" ++
            " l0: %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = extractvalue {i8*,i8**} %5,0" ++
            " %7 = bitcast i8* %6 to {i8,[0 x i1]}*" ++
            " %8 = select i1 1,i8 0,i8 0" ++
            " %9 = extractvalue {i8*,i8**} %5,1" ++
            " %10 = select i1 1,i8* null,i8* null;test new\n" ++
            " %11 = getelementptr i8*,i8** %9,i8 0" ++
            " store i8* %10,i8** %11" ++
            " %12 = select i1 1,i8 0,i8 0" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = add i8 1,%14" ++
            " store i8 %15,i8* %13" ++
            " %16 = add i8 0,%12" ++
            " %17 = getelementptr i8*,i8** %9,i8 %16" ++
            " %18 = load i8*,i8** %17;test addref\n" ++
            " br label %l1" ++
            " l1:" ++
            " %19 = phi {i8,[0 x i1]}* [%19,%l1],[%7,%l0]" ++
            " %20 = phi i8 [%20,%l1],[%8,%l0]" ++
            " %21 = phi i8** [%21,%l1],[%9,%l0]" ++
            " %22 = phi i8 [%22,%l1],[%12,%l0]" ++
            " br label %l1" ++
            " }")

testFuncallInLoop :: Assertion
testFuncallInLoop =
    testCodeGen "testFuncallInLoop" "import type test{}func testFuncallInLoop()test{for{testFuncallInLoop()}}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8,i8**,i8} @_testFuncallInLoop({i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " br label %l1" ++
            " l1:" ++
            " %6 = extractvalue {i8*,i8**} %5,0" ++
            " call void @llvm.memset.p0i8.i8(i8* %6,i8 0,i8 %1,i32 0,i1 0)" ++
            " %7 = call {{i8,[0 x i1]}*,i8,i8**,i8} @_testFuncallInLoop({i8*,i8**} %5)" ++
            " %8 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,0" ++
            " %9 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,1" ++
            " %10 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,2" ++
            " %11 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,3" ++
            " %12 = add i8 0,%11" ++
            " %13 = getelementptr i8*,i8** %10,i8 %12" ++
            " %14 = load i8*,i8** %13;test unref\n" ++
            " br label %l1" ++
            " }")

unrefStackDefn :: String
unrefStackDefn = "define void @unrefStack(i8* %s) {" ++
            " l0:" ++
            " %0 = bitcast i8* %s to {i32,i32,i32,i8*}*" ++
            " %1 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %0,i32 0,i32 0" ++
            " %2 = load i32,i32* %1" ++
            " %3 = sub i32 %2,1" ++
            " store i32 %3,i32* %1" ++
            " %4 = icmp ugt i32 %3,0" ++
            " br i1 %4,label %l1,label %l2" ++
            " l1:" ++
            " ret void" ++
            " l2:" ++
            " call void @free(i8* %s)" ++
            " ret void" ++
            " }"

testStack =
    testCodeGen "testStack" "import type stack{}func testStack(){var s stack}"
        ("declare i8* @malloc(i32)" ++
            "declare void @free(i8*)" ++
            "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)" ++
            memsetDecl ++
            unrefStackDefn ++
            copyDefn ++
            copyrttDefn ++
            "define void @_testStack() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = alloca i8*,i8 1" ++
            " %4 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = extractvalue {i8*,i8**} %5,0" ++
            " %7 = bitcast i8* %6 to {i8,[0 x i1]}*" ++
            " %8 = select i1 1,i8 0,i8 0" ++
            " %9 = extractvalue {i8*,i8**} %5,1" ++
            " %10 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* null,i32 1" ++
            " %11 = ptrtoint {i32,i32,i32,i8*}* %10 to i32" ++
            " %12 = call i8* @malloc(i32 %11)" ++
            " call void @llvm.memset.p0i8.i32(i8* %12,i8 0,i32 0,i32 0,i1 0)" ++
            " %13 = getelementptr i8*,i8** %9,i8 0" ++
            " store i8* %12,i8** %13" ++
            " %14 = select i1 1,i8 0,i8 0" ++
            " %15 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %16 = load i8,i8* %15" ++
            " %17 = add i8 1,%16" ++
            " store i8 %17,i8* %15" ++
            " %18 = add i8 0,%14" ++
            " %19 = getelementptr i8*,i8** %9,i8 %18" ++
            " %20 = load i8*,i8** %19" ++
            " %21 = bitcast i8* %20 to {i32,i32,i32,i8*}*" ++
            " %22 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %21,i32 0,i32 0" ++
            " %23 = load i32,i32* %22" ++
            " %24 = add i32 1,%23" ++
            " store i32 %24,i32* %22" ++
            " %25 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %26 = load i8,i8* %25" ++
            " %27 = sub i8 %26,1" ++
            " store i8 %27,i8* %25" ++
            " %28 = add i8 0,%14" ++
            " %29 = getelementptr i8*,i8** %9,i8 %28" ++
            " %30 = load i8*,i8** %29" ++
            " call void @unrefStack(i8* %30)" ++
            " ret void" ++
            " }")

isEmptyStackDefn :: String
isEmptyStackDefn = "define {{i8,[0 x i1]}*,i8} @_isEmptyStack({i8,[0 x i1]}* %stackvalue,i8 %stackoffset,i8** %stackimp,i8 %stackimpoffset,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = extractvalue {i8*,i8**} %retval,0" ++
            " %1 = bitcast i8* %0 to {i8,[0 x i1]}*" ++
            " %2 = getelementptr i8*,i8** %stackimp,i8 %stackimpoffset" ++
            " %3 = load i8*,i8** %2" ++
            " %4 = bitcast i8* %3 to {i32,i32,i32,i8*}*" ++
            " %5 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 1" ++
            " %6 = load i32,i32* %5" ++
            " %7 = icmp eq i32 %6,0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %1,i32 0,i32 1,i8 0" ++
            " store i1 %7,i1* %8" ++
            " %9 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %1,0" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} %9,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %10" ++
            " }"

testStackFuncs =
    testCodeGen "testStackFuncs" "import type stack{}type bit{bit}import func pushStack(s stack,b bit)import func popStack(s stack)bit import func isEmptyStack(s stack)bit"
        ("declare i8* @malloc(i32)" ++
            "declare void @free(i8*)" ++
            "declare void @llvm.memcpy.p0i8.p0i8.i32(i8*,i8*,i32,i32,i1)" ++
            "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)" ++
            memsetDecl ++
            unrefStackDefn ++
            copyDefn ++
            copyrttDefn ++
            isEmptyStackDefn ++
            "define {{i8,[0 x i1]}*,i8} @_popStack({i8,[0 x i1]}* %stackvalue,i8 %stackoffset,i8** %stackimp,i8 %stackimpoffset,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = extractvalue {i8*,i8**} %retval,0" ++
            " %1 = bitcast i8* %0 to {i8,[0 x i1]}*" ++
            " %2 = getelementptr i8*,i8** %stackimp,i8 %stackimpoffset" ++
            " %3 = load i8*,i8** %2" ++
            " %4 = bitcast i8* %3 to {i32,i32,i32,i8*}*" ++
            " %5 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 1" ++
            " %6 = load i32,i32* %5" ++
            " %7 = icmp ugt i32 %6,0" ++
            " br i1 %7,label %l1,label %l2" ++
            " l1:" ++
            " %8 = sub i32 %6,1" ++
            " store i32 %8,i32* %5" ++
            " %9 = udiv i32 %8,8" ++
            " %10 = urem i32 %8,8" ++
            " %11 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 3" ++
            " %12 = load i8*,i8** %11" ++
            " %13 = getelementptr i8,i8* %12,i32 %9" ++
            " %14 = load i8,i8* %13" ++
            " %15 = zext i8 %14 to i32" ++
            " %16 = lshr i32 %15,%10" ++
            " %17 = trunc i32 %16 to i1" ++
            " %18 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %1,i32 0,i32 1,i8 0" ++
            " store i1 %17,i1* %18" ++
            " br label %l2" ++
            " l2:" ++
            " %19 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %1,0" ++
            " %20 = insertvalue {{i8,[0 x i1]}*,i8} %19,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %20" ++
            " }" ++
            "define void @_pushStack({i8,[0 x i1]}* %stackvalue,i8 %stackoffset,i8** %stackimp,i8 %stackimpoffset,{i8,[0 x i1]}* %bitvalue,i8 %bitoffset) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %bitvalue,i32 0,i32 1,i8 %bitoffset" ++
            " %1 = load i1,i1* %0" ++
            " %2 = getelementptr i8*,i8** %stackimp,i8 %stackimpoffset" ++
            " %3 = load i8*,i8** %2" ++
            " %4 = bitcast i8* %3 to {i32,i32,i32,i8*}*" ++
            " %5 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 1" ++
            " %6 = load i32,i32* %5" ++
            " %7 = add i32 1,%6" ++
            " store i32 %7,i32* %5" ++
            " %8 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 2" ++
            " %9 = load i32,i32* %8" ++
            " %10 = icmp ugt i32 %7,%9" ++
            " br i1 %10,label %l1,label %l3" ++
            " l1:" ++
            " %11 = udiv i32 %9,8" ++
            " %12 = add i32 %11,128" ++
            " %13 = shl i32 %12,3" ++
            " %14 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 3" ++
            " %15 = load i8*,i8** %14" ++
            " %16 = call i8* @malloc(i32 %12)" ++
            " store i8* %16,i8** %14" ++
            " %17 = icmp ne i32 %11,0" ++
            " br i1 %17,label %l2,label %l3" ++
            " l2:" ++
            " call void @llvm.memcpy.p0i8.p0i8.i32(i8* %16,i8* %15,i32 %11,i32 0,i1 0)" ++
            " call void @free(i8* %15)" ++
            " br label %l3" ++
            " l3:" ++
            " %18 = udiv i32 %6,8" ++
            " %19 = urem i32 %6,8" ++
            " %20 = trunc i32 %19 to i8" ++
            " %21 = zext i1 %1 to i8" ++
            " %22 = shl i8 %21,%20" ++
            " %23 = xor i8 %22,255" ++
            " %24 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %4,i32 0,i32 3" ++
            " %25 = load i8*,i8** %24" ++
            " %26 = getelementptr i8,i8* %25,i32 %18" ++
            " %27 = load i8,i8* %26" ++
            " %28 = and i8 %23,%27" ++
            " %29 = or i8 %28,%22" ++
            " store i8 %29,i8* %26" ++
            " ret void" ++
            " }")

testStackFuncs2 =
    testCodeGen "testStackFuncs2" "import type stack{}type bit{bit}import func isEmptyStack(s stack)bit func testStackFuncs2(){var s stack;isEmptyStack(s)}"
        ("declare i8* @malloc(i32)" ++
            "declare void @free(i8*)" ++
            "declare void @llvm.memset.p0i8.i32(i8*,i8,i32,i32,i1)" ++
            memsetDecl ++
            unrefStackDefn ++
            copyDefn ++
            copyrttDefn ++
            isEmptyStackDefn ++
            "define void @_testStackFuncs2() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 0" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %3 = ptrtoint i1* %2 to i8" ++
            " %4 = alloca i8,i8 %1" ++
            " %5 = alloca i8*,i8 1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %4,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** %5,1" ++
            " %8 = alloca i8,i8 %3" ++
            " %9 = insertvalue {i8*,i8**} undef,i8* %8,0" ++
            " %10 = insertvalue {i8*,i8**} %9,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %4,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %8,i8 0,i8 %3,i32 0,i1 0)" ++
            " %11 = extractvalue {i8*,i8**} %7,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
            " %14 = extractvalue {i8*,i8**} %7,1" ++
            " %15 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* null,i32 1" ++
            " %16 = ptrtoint {i32,i32,i32,i8*}* %15 to i32" ++
            " %17 = call i8* @malloc(i32 %16)" ++
            " call void @llvm.memset.p0i8.i32(i8* %17,i8 0,i32 0,i32 0,i1 0)" ++
            " %18 = getelementptr i8*,i8** %14,i8 0" ++
            " store i8* %17,i8** %18" ++
            " %19 = select i1 1,i8 0,i8 0" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %12,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = add i8 1,%21" ++
            " store i8 %22,i8* %20" ++
            " %23 = add i8 0,%19" ++
            " %24 = getelementptr i8*,i8** %14,i8 %23" ++
            " %25 = load i8*,i8** %24" ++
            " %26 = bitcast i8* %25 to {i32,i32,i32,i8*}*" ++
            " %27 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %26,i32 0,i32 0" ++
            " %28 = load i32,i32* %27" ++
            " %29 = add i32 1,%28" ++
            " store i32 %29,i32* %27" ++
            " %30 = add i8 0,%19" ++
            " %31 = getelementptr i8*,i8** %14,i8 %30" ++
            " %32 = load i8*,i8** %31" ++
            " %33 = bitcast i8* %32 to {i32,i32,i32,i8*}*" ++
            " %34 = getelementptr {i32,i32,i32,i8*},{i32,i32,i32,i8*}* %33,i32 0,i32 0" ++
            " %35 = load i32,i32* %34" ++
            " %36 = add i32 1,%35" ++
            " store i32 %36,i32* %34" ++
            " %37 = call {{i8,[0 x i1]}*,i8} @_isEmptyStack({i8,[0 x i1]}* %12,i8 %13,i8** %14,i8 %19,{i8*,i8**} %10)" ++
            " %38 = add i8 0,%19" ++
            " %39 = getelementptr i8*,i8** %14,i8 %38" ++
            " %40 = load i8*,i8** %39" ++
            " call void @unrefStack(i8* %40)" ++
            " %41 = extractvalue {{i8,[0 x i1]}*,i8} %37,0" ++
            " %42 = extractvalue {{i8,[0 x i1]}*,i8} %37,1" ++
            " %43 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %12,i32 0,i32 0" ++
            " %44 = load i8,i8* %43" ++
            " %45 = sub i8 %44,1" ++
            " store i8 %45,i8* %43" ++
            " %46 = add i8 0,%19" ++
            " %47 = getelementptr i8*,i8** %14,i8 %46" ++
            " %48 = load i8*,i8** %47" ++
            " call void @unrefStack(i8* %48)" ++
            " ret void" ++
            " }")
