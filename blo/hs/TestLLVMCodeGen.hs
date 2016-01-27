module TestLLVMCodeGen
    (tests)
where

import Test.HUnit(Assertion,Test(..))

import TestLLVM(alloc2,prologue,testCodeGen)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testSimple2,
    TestCase testInfiniteLoop,
    TestCase testBreak,
    TestCase testBreak2,
    TestCase testBreak3,
    TestCase testBreak4,
    TestCase testSetClear,
    TestCase testIf,
    TestCase testIf2,
    TestCase testIf3,
    TestCase testIf4,
    TestCase testIf5,
    TestCase testIf6,
    TestCase testIf7,
    TestCase testScope,
    TestCase testScope2,
    TestCase testScope3,
    TestCase testScope4,
    TestCase testScope5,
    TestCase testScope6,
    TestCase testScope7,
    TestCase testAssign,
    TestCase testAssign2,
    TestCase testFuncall,
    TestCase testVarInitializer,
    TestCase testVarInitializer2,
    TestCase testVarInLoop,
    TestCase testVarInLoop2,
    TestCase testVarInLoop3
    ]

testSimple :: Assertion
testSimple =
    testCodeGen "testSimple" "type a{a}func TestSimple()a{var r a;return r}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8} @_TestSimple({i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = sub i8 %12,1" ++
            " store i8 %13,i8* %11" ++
            " %14 = extractvalue {i8*,i8**} %retval,0" ++
            " %15 = bitcast i8* %14 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %6,i8 %7,{i8,[0 x i1]}* %15,i8 0,i8 1)" ++
            " %16 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %15,0" ++
            " %17 = insertvalue {{i8,[0 x i1]}*,i8} %16,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %17" ++
            " }")

testSimple2 :: Assertion
testSimple2 =
    testCodeGen "testSimple2" "type a{a}func TestSimple2(a a)a{return a}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8} @_TestSimple2({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %6 = load i8,i8* %5" ++
            " %7 = sub i8 %6,1" ++
            " store i8 %7,i8* %5" ++
            " %8 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %8, label %l1, label %l2" ++
            " l1:" ++
            " %9 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} %9,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %10" ++
            " l2:" ++
            " %11 = extractvalue {i8*,i8**} %retval,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %12,i8 0,i8 1)" ++
            " %13 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %12,0" ++
            " %14 = insertvalue {{i8,[0 x i1]}*,i8} %13,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %14" ++
            " }")

testInfiniteLoop :: Assertion
testInfiniteLoop =
    testCodeGen "testInfiniteLoop" "func TestInfiniteLoop() { for {} }"
        (prologue ++
            "define void @_TestInfiniteLoop() {" ++
            " l0:" ++
            " br label %l1" ++
            " l1:" ++
            " br label %l1" ++
            " }")

testBreak :: Assertion
testBreak =
    testCodeGen "testBreak" "func TestBreak(){for{break}}"
        (prologue ++
            "define void @_TestBreak() {" ++
            " l0:" ++
            " br label %l1" ++
            " l1:" ++
            " ret void" ++
            " }")

testBreak2 :: Assertion
testBreak2 =
    testCodeGen "testBreak2" "func TestBreak2(){for{for{break}}}"
        (prologue ++
            "define void @_TestBreak2() {" ++
            " l0:" ++
            " br label %l1" ++
            " l1:" ++
            " br label %l2" ++
            " l2:" ++
            " br label %l1" ++
            " }")

testBreak3 :: Assertion
testBreak3 =
    testCodeGen "testBreak3" "func TestBreak3(){for a{for{break a}}}"
        (prologue ++
            "define void @_TestBreak3() {" ++
            " l0:" ++
            " br label %l1" ++
            " l1:" ++
            " br label %l2" ++
            " l2:" ++
            " ret void" ++
            " }")

testBreak4 :: Assertion
testBreak4 =
    testCodeGen "testBreak4" "func TestBreak4(){for a{for{break a}}for{}}"
        (prologue ++
            "define void @_TestBreak4() {" ++
            " l0:" ++
            " br label %l1" ++
            " l1:" ++
            " br label %l2" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " br label %l3" ++
            " }")

testSetClear :: Assertion
testSetClear =
    testCodeGen "testSetClear" "type a{a}func TestSetClear(){var a a;set a.a;clear a.a}"
        (prologue ++
            "define void @_TestSetClear() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %11" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 0,i1* %12" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = sub i8 %14,1" ++
            " store i8 %15,i8* %13" ++
            " ret void" ++
            " }")

testIf :: Assertion
testIf =
    testCodeGen "testIf" "type a{a}func TestIf(a a){if a.a{set a.a}}"
        (prologue ++
            "define void @_TestIf({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %7" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = sub i8 %9,1" ++
            " store i8 %10,i8* %8" ++
            " ret void" ++
            " l2:" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = sub i8 %12,1" ++
            " store i8 %13,i8* %11" ++
            " ret void" ++
            " }")

testIf2 :: Assertion
testIf2 =
    testCodeGen "testIf2" "type a{a}func TestIf2(a a){if a.a{set a.a}else{clear a.a}}"
        (prologue ++
            "define void @_TestIf2({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %7" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = sub i8 %9,1" ++
            " store i8 %10,i8* %8" ++
            " ret void" ++
            " l2:" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 0,i1* %11" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %13 = load i8,i8* %12" ++
            " %14 = sub i8 %13,1" ++
            " store i8 %14,i8* %12" ++
            " ret void" ++
            " }")

testIf3 :: Assertion
testIf3 =
    testCodeGen "testIf3" "type a{a}func TestIf3(a,b a){if a.a{set a.a}else if b.a{clear a.a}}"
        (prologue ++
            "define void @_TestIf3({i8,[0 x i1]}* %value0,i8 %offset0,{i8,[0 x i1]}* %value1,i8 %offset1) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = select i1 1,{i8,[0 x i1]}* %value1,{i8,[0 x i1]}* null" ++
            " %6 = select i1 1,i8 %offset1,i8 0" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 0" ++
            " %8 = load i8,i8* %7" ++
            " %9 = add i8 1,%8" ++
            " store i8 %9,i8* %7" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %11 = load i1,i1* %10" ++
            " br i1 %11, label %l1, label %l2" ++
            " l1:" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %12" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = sub i8 %14,1" ++
            " store i8 %15,i8* %13" ++
            " %16 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 0" ++
            " %17 = load i8,i8* %16" ++
            " %18 = sub i8 %17,1" ++
            " store i8 %18,i8* %16" ++
            " ret void" ++
            " l2:" ++
            " %19 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 1,i8 %6" ++
            " %20 = load i1,i1* %19" ++
            " br i1 %20, label %l3, label %l4" ++
            " l3:" ++
            " %21 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 0,i1* %21" ++
            " %22 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %23 = load i8,i8* %22" ++
            " %24 = sub i8 %23,1" ++
            " store i8 %24,i8* %22" ++
            " %25 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 0" ++
            " %26 = load i8,i8* %25" ++
            " %27 = sub i8 %26,1" ++
            " store i8 %27,i8* %25" ++
            " ret void" ++
            " l4:" ++
            " %28 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %29 = load i8,i8* %28" ++
            " %30 = sub i8 %29,1" ++
            " store i8 %30,i8* %28" ++
            " %31 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 0" ++
            " %32 = load i8,i8* %31" ++
            " %33 = sub i8 %32,1" ++
            " store i8 %33,i8* %31" ++
            " ret void" ++
            " }")

testIf4 :: Assertion
testIf4 =
    testCodeGen "testIf4" "type a{a}func TestIf4(a a){if a.a{set a.a}clear a.a}"
        (prologue ++
            "define void @_TestIf4({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %7" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %8 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %9 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %8,i32 0,i32 1,i8 %9" ++
            " store i1 0,i1* %10" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %8,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = sub i8 %12,1" ++
            " store i8 %13,i8* %11" ++
            " ret void" ++
            " }")

testIf5 :: Assertion
testIf5 =
    testCodeGen "testIf5" "type a{a}func TestIf5(a a){if a.a{}clear a.a}"
        (prologue ++
            "define void @_TestIf5({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3: %7 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %8 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 1,i8 %8" ++
            " store i1 0,i1* %9" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %11 = load i8,i8* %10" ++
            " %12 = sub i8 %11,1" ++
            " store i8 %12,i8* %10" ++
            " ret void" ++
            " }")

testIf6 :: Assertion
testIf6 =
    testCodeGen "testIf6" "type a{a}func TestIf6(a a){if a.a{}else{}clear a.a}"
        (prologue ++
            "define void @_TestIf6({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3: %7 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %8 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 1,i8 %8" ++
            " store i1 0,i1* %9" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %11 = load i8,i8* %10" ++
            " %12 = sub i8 %11,1" ++
            " store i8 %12,i8* %10" ++
            " ret void" ++
            " }")

testIf7 :: Assertion
testIf7 =
    testCodeGen "testIf7" "type a{a}func TestIf7(a a){if a.a{}else{return}clear a.a}"
        (prologue ++
            "define void @_TestIf7({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %6 = load i1,i1* %5" ++
            " br i1 %6, label %l1, label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %8 = load i8,i8* %7" ++
            " %9 = sub i8 %8,1" ++
            " store i8 %9,i8* %7" ++
            " ret void" ++
            " l3:" ++
            " %10 = phi {i8,[0 x i1]}* [%0,%l1]" ++
            " %11 = phi i8 [%1,%l1]" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %10,i32 0,i32 1,i8 %11" ++
            " store i1 0,i1* %12" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %10,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = sub i8 %14,1" ++
            " store i8 %15,i8* %13" ++
            " ret void" ++
            " }")

testScope :: Assertion
testScope =
    testCodeGen "testScope" "type a{a}func TestScope(){{var a a}var a a}"
        (prologue ++
            "define void @_TestScope() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = add i8 1,%12" ++
            " store i8 %13,i8* %11" ++
            " %14 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %15 = load i8,i8* %14" ++
            " %16 = sub i8 %15,1" ++
            " store i8 %16,i8* %14" ++
            " %17 = extractvalue {i8*,i8**} %7,0" ++
            " %18 = bitcast i8* %17 to {i8,[0 x i1]}*" ++
            " %19 = select i1 1,i8 0,i8 0" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = add i8 1,%21" ++
            " store i8 %22,i8* %20" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " ret void" ++
            " }")

testScope2 :: Assertion
testScope2 =
    testCodeGen "testScope2" "type a{a}func TestScope2(){for{var a a}}"
        (prologue ++
            "define void @_TestScope2() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " br label %l1" ++
            " l1:" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %6 = extractvalue {i8*,i8**} %4,0" ++
            " %7 = bitcast i8* %6 to {i8,[0 x i1]}*" ++
            " %8 = select i1 1,i8 0,i8 0" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %10 = load i8,i8* %9" ++
            " %11 = add i8 1,%10" ++
            " store i8 %11,i8* %9" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %7,i32 0,i32 0" ++
            " %13 = load i8,i8* %12" ++
            " %14 = sub i8 %13,1" ++
            " store i8 %14,i8* %12" ++
            " br label %l1" ++
            " }")

testScope3 :: Assertion
testScope3 =
    testCodeGen "testScope3" "type a{a}func TestScope3(){var a a{set a.a}{set a.a}}"
        (prologue ++
            "define void @_TestScope3() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %11" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %12" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = sub i8 %14,1" ++
            " store i8 %15,i8* %13" ++
            " ret void" ++
            " }")

testScope4 :: Assertion
testScope4 =
    testCodeGen "testScope4" "type a{a}func TestScope4(){var a a{set a.a}if a.a{set a.a}}"
        (prologue ++
            "define void @_TestScope4() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %11" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %13 = load i1,i1* %12" ++
            " br i1 %13, label %l1, label %l2" ++
            " l1:" ++
            " %14 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %14" ++
            " %15 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %16 = load i8,i8* %15" ++
            " %17 = sub i8 %16,1" ++
            " store i8 %17,i8* %15" ++
            " ret void" ++
            " l2:" ++
            " %18 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %19 = load i8,i8* %18" ++
            " %20 = sub i8 %19,1" ++
            " store i8 %20,i8* %18" ++
            " ret void" ++
            " }")

testScope5 :: Assertion
testScope5 =
    testCodeGen "testScope5" "type a{a}func TestScope5(){var a a;if a.a{set a.a}{set a.a}}"
        (prologue ++
            "define void @_TestScope5() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %12 = load i1,i1* %11" ++
            " br i1 %12, label %l1, label %l2" ++
            " l1:" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %13" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %14 = phi {i8,[0 x i1]}* [%6,%l2],[%6,%l1]" ++
            " %15 = phi i8 [%7,%l2],[%7,%l1]" ++
            " %16 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 1,i8 %15" ++
            " store i1 1,i1* %16" ++
            " %17 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 0" ++
            " %18 = load i8,i8* %17" ++
            " %19 = sub i8 %18,1" ++
            " store i8 %19,i8* %17" ++
            " ret void" ++
            " }")

testScope6 :: Assertion
testScope6 =
    testCodeGen "testScope6" "type a{a}func TestScope6(){var a a;if a.a{set a.a}if a.a{set a.a}}"
        (prologue ++
            "define void @_TestScope6() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %12 = load i1,i1* %11" ++
            " br i1 %12, label %l1, label %l2" ++
            " l1:" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %13" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %14 = phi {i8,[0 x i1]}* [%6,%l2],[%6,%l1]" ++
            " %15 = phi i8 [%7,%l2],[%7,%l1]" ++
            " %16 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 1,i8 %15" ++
            " %17 = load i1,i1* %16" ++
            " br i1 %17, label %l4, label %l5" ++
            " l4:" ++
            " %18 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 1,i8 %15" ++
            " store i1 1,i1* %18" ++
            " %19 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 0" ++
            " %20 = load i8,i8* %19" ++
            " %21 = sub i8 %20,1" ++
            " store i8 %21,i8* %19" ++
            " ret void" ++
            " l5:" ++
            " %22 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %14,i32 0,i32 0" ++
            " %23 = load i8,i8* %22" ++
            " %24 = sub i8 %23,1" ++
            " store i8 %24,i8* %22" ++
            " ret void" ++
            " }")

testScope7 :: Assertion
testScope7 =
    testCodeGen "testScope7" "type a{a}func TestScope7(){var a a;if a.a{return}set a.a}"
        (prologue ++
            "define void @_TestScope7() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %12 = load i1,i1* %11" ++
            " br i1 %12, label %l1, label %l2" ++
            " l1:" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %14 = load i8,i8* %13" ++
            " %15 = sub i8 %14,1" ++
            " store i8 %15,i8* %13" ++
            " ret void" ++
            " l2: br label %l3" ++
            " l3:" ++
            " %16 = phi {i8,[0 x i1]}* [%6,%l2] %17 = phi i8 [%7,%l2]" ++
            " %18 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %16,i32 0,i32 1,i8 %17" ++
            " store i1 1,i1* %18" ++
            " %19 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %16,i32 0,i32 0" ++
            " %20 = load i8,i8* %19" ++
            " %21 = sub i8 %20,1" ++
            " store i8 %21,i8* %19" ++
            " ret void" ++
            " }")

testAssign :: Assertion
testAssign =
    testCodeGen "testAssign" "type a{a}func TestAssign(){var a a;var b a;b=a}"
        (prologue ++
            "define void @_TestAssign() {" ++
            " l0: %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = add i8 1,%12" ++
            " store i8 %13,i8* %11" ++
            " %14 = extractvalue {i8*,i8**} %7,0" ++
            " %15 = bitcast i8* %14 to {i8,[0 x i1]}*" ++
            " %16 = select i1 1,i8 0,i8 0" ++
            " %17 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %18 = load i8,i8* %17" ++
            " %19 = add i8 1,%18" ++
            " store i8 %19,i8* %17" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = add i8 1,%21" ++
            " store i8 %22,i8* %20" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " %26 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %27 = load i8,i8* %26" ++
            " %28 = sub i8 %27,1" ++
            " store i8 %28,i8* %26" ++
            " %29 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %30 = load i8,i8* %29" ++
            " %31 = sub i8 %30,1" ++
            " store i8 %31,i8* %29" ++
            " ret void" ++
            " }")

testAssign2 :: Assertion
testAssign2 =
    testCodeGen "testAssign2" "type a{a}func TestAssign2(){var a a;var b a;b.a=a.a}"
        (prologue ++
            "define void @_TestAssign2() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = add i8 1,%12" ++
            " store i8 %13,i8* %11" ++
            " %14 = extractvalue {i8*,i8**} %7,0" ++
            " %15 = bitcast i8* %14 to {i8,[0 x i1]}*" ++
            " %16 = select i1 1,i8 0,i8 0" ++
            " %17 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %18 = load i8,i8* %17" ++
            " %19 = add i8 1,%18" ++
            " store i8 %19,i8* %17" ++
            " call void @copy({i8,[0 x i1]}* %9,i8 %10,{i8,[0 x i1]}* %15,i8 %16,i8 1)" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = sub i8 %21,1" ++
            " store i8 %22,i8* %20" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " ret void" ++
            " }")

testFuncall :: Assertion
testFuncall =
    testCodeGen "testFuncall" "type a{a}func id(a a)a{return a}func TestFuncall(){var a a;a=id(a)}"
        (prologue ++
            "define void @_TestFuncall() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = add i8 1,%12" ++
            " store i8 %13,i8* %11" ++
            " %14 = call {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %9,i8 %10,{i8*,i8**} %7)" ++
            " %15 = extractvalue {{i8,[0 x i1]}*,i8} %14,0" ++
            " %16 = extractvalue {{i8,[0 x i1]}*,i8} %14,1" ++
            " %17 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %18 = load i8,i8* %17" ++
            " %19 = add i8 1,%18" ++
            " store i8 %19,i8* %17" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = sub i8 %21,1" ++
            " store i8 %22,i8* %20" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %15,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " ret void" ++
            " }" ++
            "define {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %6 = load i8,i8* %5" ++
            " %7 = sub i8 %6,1" ++
            " store i8 %7,i8* %5" ++
            " %8 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %8, label %l1, label %l2" ++
            " l1:" ++
            " %9 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} %9,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %10" ++
            " l2:" ++
            " %11 = extractvalue {i8*,i8**} %retval,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %12,i8 0,i8 1)" ++
            " %13 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %12,0" ++
            " %14 = insertvalue {{i8,[0 x i1]}*,i8} %13,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %14" ++
            " }")

testVarInitializer :: Assertion
testVarInitializer =
    testCodeGen "testVarInitializer" "type a{a}func TestVarInitializer(){var a a;var b a=a}"
        (prologue ++
            "define void @_TestVarInitializer() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %12 = load i8,i8* %11" ++
            " %13 = add i8 1,%12" ++
            " store i8 %13,i8* %11" ++
            " %14 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %15 = load i8,i8* %14" ++
            " %16 = add i8 1,%15" ++
            " store i8 %16,i8* %14" ++
            " %17 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %18 = load i8,i8* %17" ++
            " %19 = sub i8 %18,1" ++
            " store i8 %19,i8* %17" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %9,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = sub i8 %21,1" ++
            " store i8 %22,i8* %20" ++
            " ret void" ++
            " }")

testVarInitializer2 :: Assertion
testVarInitializer2 =
    testCodeGen "testVarInitializer2" "type a{a}func id(a a)a{return a}func TestVarInitializer2(){var a a;var b a=id(a)}"
        (prologue ++
            "define void @_TestVarInitializer2() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " %8 = alloca i8,i8 %1" ++
            " %9 = insertvalue {i8*,i8**} undef,i8* %8,0" ++
            " %10 = insertvalue {i8*,i8**} %9,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %8,i8 0,i8 %1,i32 0,i1 0)" ++
            " %11 = extractvalue {i8*,i8**} %4,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
            " %14 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %12,i32 0,i32 0" ++
            " %15 = load i8,i8* %14" ++
            " %16 = add i8 1,%15" ++
            " store i8 %16,i8* %14" ++
            " %17 = call {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %12,i8 %13,{i8*,i8**} %10)" ++
            " %18 = extractvalue {{i8,[0 x i1]}*,i8} %17,0" ++
            " %19 = extractvalue {{i8,[0 x i1]}*,i8} %17,1" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = add i8 1,%21" ++
            " store i8 %22,i8* %20" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " %26 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %12,i32 0,i32 0" ++
            " %27 = load i8,i8* %26" ++
            " %28 = sub i8 %27,1" ++
            " store i8 %28,i8* %26" ++
            " ret void" ++
            " }" ++
            "define {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %3 = load i8,i8* %2" ++
            " %4 = add i8 1,%3" ++
            " store i8 %4,i8* %2" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 0" ++
            " %6 = load i8,i8* %5" ++
            " %7 = sub i8 %6,1 store i8 %7,i8* %5" ++
            " %8 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %8, label %l1, label %l2" ++
            " l1:" ++
            " %9 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} %9,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %10" ++
            " l2:" ++
            " %11 = extractvalue {i8*,i8**} %retval,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %12,i8 0,i8 1)" ++
            " %13 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %12,0" ++
            " %14 = insertvalue {{i8,[0 x i1]}*,i8} %13,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %14" ++
            " }")

testVarInLoop :: Assertion
testVarInLoop =
    testCodeGen "testVarInLoop" "type a{a}func TestVarInLoop(){var a a;for{}}"
        (prologue ++
            "define void @_TestVarInLoop() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0" ++
            " %9 = load i8,i8* %8" ++
            " %10 = add i8 1,%9" ++
            " store i8 %10,i8* %8" ++
            " br label %l1" ++
            " l1:" ++
            " %11 = phi {i8,[0 x i1]}* [%11,%l1],[%6,%l0]" ++
            " %12 = phi i8 [%12,%l1],[%7,%l0]" ++
            " br label %l1" ++
            " }")

testVarInLoop2 :: Assertion
testVarInLoop2 =
    testCodeGen "testVarInLoop2" "type a{a}func TestVarInLoop2(){var a a;for{var b a;a=b}}"
        (prologue ++ alloc2 ++
            "define void @_TestVarInLoop2() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " %8 = alloca i8,i8 %1" ++
            " %9 = insertvalue {i8*,i8**} undef,i8* %8,0" ++
            " %10 = insertvalue {i8*,i8**} %9,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %8,i8 0,i8 %1,i32 0,i1 0)" ++
            " %11 = extractvalue {i8*,i8**} %4,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
            " %14 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %12,i32 0,i32 0" ++
            " %15 = load i8,i8* %14" ++
            " %16 = add i8 1,%15" ++
            " store i8 %16,i8* %14" ++
            " br label %l1" ++
            " l1:" ++
            " %17 = phi {i8,[0 x i1]}* [%22,%l1],[%12,%l0]" ++
            " %18 = phi i8 [%23,%l1],[%13,%l0]" ++
            " %19 = call {i8*,i8**} @alloc2({i8*,i8**} %7,{i8*,i8**} %10)" ++
            " %20 = extractvalue {i8*,i8**} %19,0" ++
            " call void @llvm.memset.p0i8.i8(i8* %20,i8 0,i8 %1,i32 0,i1 0)" ++
            " %21 = extractvalue {i8*,i8**} %19,0" ++
            " %22 = bitcast i8* %21 to {i8,[0 x i1]}*" ++
            " %23 = select i1 1,i8 0,i8 0" ++
            " %24 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %22,i32 0,i32 0" ++
            " %25 = load i8,i8* %24" ++
            " %26 = add i8 1,%25" ++
            " store i8 %26,i8* %24" ++
            " %27 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %22,i32 0,i32 0" ++
            " %28 = load i8,i8* %27" ++
            " %29 = add i8 1,%28" ++
            " store i8 %29,i8* %27" ++
            " %30 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %17,i32 0,i32 0" ++
            " %31 = load i8,i8* %30" ++
            " %32 = sub i8 %31,1" ++
            " store i8 %32,i8* %30" ++
            " %33 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %22,i32 0,i32 0" ++
            " %34 = load i8,i8* %33" ++
            " %35 = sub i8 %34,1" ++
            " store i8 %35,i8* %33" ++
            " br label %l1" ++
            " }")

testVarInLoop3 :: Assertion
testVarInLoop3 =
    testCodeGen "testVarInLoop3" "type a{a}func TestVarInLoop3(a a){for{var b a;a.a=b.a}}"
        (prologue ++ alloc2 ++
            "define void @_TestVarInLoop3({i8,[0 x i1]}* %value0,i8 %offset0) {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " %5 = alloca i8,i8 %1" ++
            " %6 = insertvalue {i8*,i8**} undef,i8* %5,0" ++
            " %7 = insertvalue {i8*,i8**} %6,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %9 = select i1 1,i8 %offset0,i8 0" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %8,i32 0,i32 0" ++
            " %11 = load i8,i8* %10" ++
            " %12 = add i8 1,%11" ++
            " store i8 %12,i8* %10" ++
            " br label %l1" ++
            " l1:" ++
            " %13 = phi {i8,[0 x i1]}* [%13,%l1],[%8,%l0]" ++
            " %14 = phi i8 [%14,%l1],[%9,%l0]" ++
            " %15 = call {i8*,i8**} @alloc2({i8*,i8**} %4,{i8*,i8**} %7)" ++
            " %16 = extractvalue {i8*,i8**} %15,0" ++
            " call void @llvm.memset.p0i8.i8(i8* %16,i8 0,i8 %1,i32 0,i1 0)" ++
            " %17 = extractvalue {i8*,i8**} %15,0" ++
            " %18 = bitcast i8* %17 to {i8,[0 x i1]}*" ++
            " %19 = select i1 1,i8 0,i8 0" ++
            " %20 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %21 = load i8,i8* %20" ++
            " %22 = add i8 1,%21" ++
            " store i8 %22,i8* %20" ++
            " call void @copy({i8,[0 x i1]}* %18,i8 %19,{i8,[0 x i1]}* %13,i8 %14,i8 1)" ++
            " %23 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %18,i32 0,i32 0" ++
            " %24 = load i8,i8* %23" ++
            " %25 = sub i8 %24,1" ++
            " store i8 %25,i8* %23" ++
            " br label %l1" ++
            " }")
