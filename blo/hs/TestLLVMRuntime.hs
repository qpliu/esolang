module TestLLVMRuntime
    (tests)
where

import Test.HUnit(Assertion,Test(..))

import TestLLVM(testCodeGen,prologue)

tests :: Test
tests = TestList [
    TestCase testVar,
    TestCase testAssign,
    TestCase testReturn,
    TestCase testFuncall,
    TestCase testVarInLoop,
    TestCase testFuncallInLoop
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
            " %5 = insertvalue {i8*,i8**} %4,i8** %3,1 call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0) br label %l1 l1: %6 = extractvalue {i8*,i8**} %5,0 call void @llvm.memset.p0i8.i8(i8* %6,i8 0,i8 %1,i32 0,i1 0) %7 = call {{i8,[0 x i1]}*,i8,i8**,i8} @_testFuncallInLoop({i8*,i8**} %5) %8 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,0 %9 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,1 %10 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,2 %11 = extractvalue {{i8,[0 x i1]}*,i8,i8**,i8} %7,3 %12 = add i8 0,%11 %13 = getelementptr i8*,i8** %10,i8 %12 %14 = load i8*,i8** %13;test unref\n br label %l1 }")
