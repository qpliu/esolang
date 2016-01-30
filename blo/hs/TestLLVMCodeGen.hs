module TestLLVMCodeGen
    (tests)
where

import Test.HUnit(Assertion,Test(..))

import TestLLVM(testCodeGen,prologue,alloc2Defn)

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
    TestCase testFuncall2,
    TestCase testFuncall3,
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
            " %8 = extractvalue {i8*,i8**} %retval,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %6,i8 %7,{i8,[0 x i1]}* %9,i8 0,i8 1)" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %9,0" ++
            " %11 = insertvalue {{i8,[0 x i1]}*,i8} %10,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %11" ++
            " }")

testSimple2 :: Assertion
testSimple2 =
    testCodeGen "testSimple2" "type a{a}func TestSimple2(a a)a{return a}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8} @_TestSimple2({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %2,label %l1,label %l2" ++
            " l1:" ++
            " %3 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %4 = insertvalue {{i8,[0 x i1]}*,i8} %3,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %4" ++
            " l2:" ++
            " %5 = extractvalue {i8*,i8**} %retval,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %6,i8 0,i8 1)" ++
            " %7 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %6,0" ++
            " %8 = insertvalue {{i8,[0 x i1]}*,i8} %7,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %8" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %8" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 0,i1* %9" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " %4 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %4" ++
            " ret void" ++
            " l2:" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " %4 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %4" ++
            " ret void" ++
            " l2:" ++
            " %5 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 0,i1* %5" ++
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
            " %2 = select i1 1,{i8,[0 x i1]}* %value1,{i8,[0 x i1]}* null" ++
            " %3 = select i1 1,i8 %offset1,i8 0" ++
            " %4 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %5 = load i1,i1* %4" ++
            " br i1 %5,label %l1,label %l2" ++
            " l1:" ++
            " %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %6" ++
            " ret void" ++
            " l2:" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %2,i32 0,i32 1,i8 %3" ++
            " %8 = load i1,i1* %7" ++
            " br i1 %8,label %l3,label %l4" ++
            " l3:" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 0,i1* %9" ++
            " ret void" ++
            " l4:" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " %4 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " store i1 1,i1* %4" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %5 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %6 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %5,i32 0,i32 1,i8 %6" ++
            " store i1 0,i1* %7" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %4 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %5 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %4,i32 0,i32 1,i8 %5" ++
            " store i1 0,i1* %6" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %4 = phi {i8,[0 x i1]}* [%0,%l2],[%0,%l1]" ++
            " %5 = phi i8 [%1,%l2],[%1,%l1]" ++
            " %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %4,i32 0,i32 1,i8 %5" ++
            " store i1 0,i1* %6" ++
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
            " %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %0,i32 0,i32 1,i8 %1" ++
            " %3 = load i1,i1* %2" ++
            " br i1 %3,label %l1,label %l2" ++
            " l1:" ++
            " br label %l3" ++
            " l2:" ++
            " ret void" ++
            " l3:" ++
            " %4 = phi {i8,[0 x i1]}* [%0,%l1]" ++
            " %5 = phi i8 [%1,%l1]" ++
            " %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %4,i32 0,i32 1,i8 %5" ++
            " store i1 0,i1* %6" ++
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
            " %11 = extractvalue {i8*,i8**} %7,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %8" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %9" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %8" ++
            " %9 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %10 = load i1,i1* %9" ++
            " br i1 %10,label %l1,label %l2" ++
            " l1:" ++
            " %11 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %11" ++
            " ret void" ++
            " l2:" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %9 = load i1,i1* %8" ++
            " br i1 %9,label %l1,label %l2" ++
            " l1:" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %10" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %11 = phi {i8,[0 x i1]}* [%6,%l2],[%6,%l1]" ++
            " %12 = phi i8 [%7,%l2],[%7,%l1]" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %11,i32 0,i32 1,i8 %12" ++
            " store i1 1,i1* %13" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %9 = load i1,i1* %8" ++
            " br i1 %9,label %l1,label %l2" ++
            " l1:" ++
            " %10 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " store i1 1,i1* %10" ++
            " br label %l3" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %11 = phi {i8,[0 x i1]}* [%6,%l2],[%6,%l1]" ++
            " %12 = phi i8 [%7,%l2],[%7,%l1]" ++
            " %13 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %11,i32 0,i32 1,i8 %12" ++
            " %14 = load i1,i1* %13" ++
            " br i1 %14,label %l4,label %l5" ++
            " l4:" ++
            " %15 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %11,i32 0,i32 1,i8 %12" ++
            " store i1 1,i1* %15" ++
            " ret void" ++
            " l5:" ++
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
            " %8 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 1,i8 %7" ++
            " %9 = load i1,i1* %8" ++
            " br i1 %9,label %l1,label %l2" ++
            " l1:" ++
            " ret void" ++
            " l2:" ++
            " br label %l3" ++
            " l3:" ++
            " %10 = phi {i8,[0 x i1]}* [%6,%l2]" ++
            " %11 = phi i8 [%7,%l2]" ++
            " %12 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %10,i32 0,i32 1,i8 %11" ++
            " store i1 1,i1* %12" ++
            " ret void" ++
            " }")

testAssign :: Assertion
testAssign =
    testCodeGen "testAssign" "type a{a}func TestAssign(){var a a;var b a;b=a}"
        (prologue ++
            "define void @_TestAssign() {" ++
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
            " %11 = extractvalue {i8*,i8**} %7,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
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
            " %11 = extractvalue {i8*,i8**} %7,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
            " call void @copy({i8,[0 x i1]}* %9,i8 %10,{i8,[0 x i1]}* %12,i8 %13,i8 1)" ++
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
            " %11 = call {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %9,i8 %10,{i8*,i8**} %7)" ++
            " %12 = extractvalue {{i8,[0 x i1]}*,i8} %11,0" ++
            " %13 = extractvalue {{i8,[0 x i1]}*,i8} %11,1" ++
            " ret void" ++
            " }" ++
            "define {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %2,label %l1,label %l2" ++
            " l1:" ++
            " %3 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %4 = insertvalue {{i8,[0 x i1]}*,i8} %3,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %4" ++
            " l2:" ++
            " %5 = extractvalue {i8*,i8**} %retval,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %6,i8 0,i8 1)" ++
            " %7 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %6,0" ++
            " %8 = insertvalue {{i8,[0 x i1]}*,i8} %7,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %8" ++
            " }")

testFuncall2 :: Assertion
testFuncall2 =
    testCodeGen "testFuncall2" "type a{a}func TestFuncall2()a{TestFuncall2();var a a;return a}"
        (prologue ++
            "define {{i8,[0 x i1]}*,i8} @_TestFuncall2({i8*,i8**} %retval) {" ++
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
            " %8 = call {{i8,[0 x i1]}*,i8} @_TestFuncall2({i8*,i8**} %4)" ++
            " %9 = extractvalue {{i8,[0 x i1]}*,i8} %8,0" ++
            " %10 = extractvalue {{i8,[0 x i1]}*,i8} %8,1" ++
            " %11 = extractvalue {i8*,i8**} %7,0" ++
            " %12 = bitcast i8* %11 to {i8,[0 x i1]}*" ++
            " %13 = select i1 1,i8 0,i8 0" ++
            " %14 = extractvalue {i8*,i8**} %retval,0" ++
            " %15 = bitcast i8* %14 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %12,i8 %13,{i8,[0 x i1]}* %15,i8 0,i8 1)" ++
            " %16 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %15,0" ++
            " %17 = insertvalue {{i8,[0 x i1]}*,i8} %16,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %17" ++
            " }")

testFuncall3 :: Assertion
testFuncall3 =
    testCodeGen "testFuncall3" "type a{a}func a()a{var a a;return a}func TestFuncall3(){a()}"
        (prologue ++
            "define void @_TestFuncall3() {" ++
            " l0:" ++
            " %0 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* null,i32 0,i32 1,i8 1" ++
            " %1 = ptrtoint i1* %0 to i8" ++
            " %2 = alloca i8,i8 %1" ++
            " %3 = insertvalue {i8*,i8**} undef,i8* %2,0" ++
            " %4 = insertvalue {i8*,i8**} %3,i8** null,1" ++
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = call {{i8,[0 x i1]}*,i8} @_a({i8*,i8**} %4)" ++
            " %6 = extractvalue {{i8,[0 x i1]}*,i8} %5,0" ++
            " %7 = extractvalue {{i8,[0 x i1]}*,i8} %5,1" ++
            " ret void" ++
            " }" ++
            "define {{i8,[0 x i1]}*,i8} @_a({i8*,i8**} %retval) {" ++
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
            " %8 = extractvalue {i8*,i8**} %retval,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %6,i8 %7,{i8,[0 x i1]}* %9,i8 0,i8 1)" ++
            " %10 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %9,0" ++
            " %11 = insertvalue {{i8,[0 x i1]}*,i8} %10,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %11" ++
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
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " %5 = extractvalue {i8*,i8**} %4,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " %7 = select i1 1,i8 0,i8 0" ++
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
            " call void @llvm.memset.p0i8.i8(i8* %2,i8 0,i8 %1,i32 0,i1 0)" ++
            " call void @llvm.memset.p0i8.i8(i8* %5,i8 0,i8 %1,i32 0,i1 0)" ++
            " %8 = extractvalue {i8*,i8**} %4,0" ++
            " %9 = bitcast i8* %8 to {i8,[0 x i1]}*" ++
            " %10 = select i1 1,i8 0,i8 0" ++
            " %11 = call {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %9,i8 %10,{i8*,i8**} %7)" ++
            " %12 = extractvalue {{i8,[0 x i1]}*,i8} %11,0" ++
            " %13 = extractvalue {{i8,[0 x i1]}*,i8} %11,1" ++
            " ret void" ++
            " }" ++
            "define {{i8,[0 x i1]}*,i8} @_id({i8,[0 x i1]}* %value0,i8 %offset0,{i8*,i8**} %retval) {" ++
            " l0:" ++
            " %0 = select i1 1,{i8,[0 x i1]}* %value0,{i8,[0 x i1]}* null" ++
            " %1 = select i1 1,i8 %offset0,i8 0" ++
            " %2 = icmp eq {i8,[0 x i1]}* %value0,%0" ++
            " br i1 %2,label %l1,label %l2" ++
            " l1:" ++
            " %3 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %0,0" ++
            " %4 = insertvalue {{i8,[0 x i1]}*,i8} %3,i8 %1,1" ++
            " ret {{i8,[0 x i1]}*,i8} %4" ++
            " l2:" ++
            " %5 = extractvalue {i8*,i8**} %retval,0" ++
            " %6 = bitcast i8* %5 to {i8,[0 x i1]}*" ++
            " call void @copy({i8,[0 x i1]}* %0,i8 %1,{i8,[0 x i1]}* %6,i8 0,i8 1)" ++
            " %7 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %6,0" ++
            " %8 = insertvalue {{i8,[0 x i1]}*,i8} %7,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %8" ++
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
            " br label %l1" ++
            " l1:" ++
            " %8 = phi {i8,[0 x i1]}* [%8,%l1],[%6,%l0]" ++
            " %9 = phi i8 [%9,%l1],[%7,%l0]" ++
            " br label %l1" ++
            " }")

testVarInLoop2 :: Assertion
testVarInLoop2 =
    testCodeGen "testVarInLoop2" "type a{a}func TestVarInLoop2(){var a a;for{var b a;a=b}}"
        (prologue ++ alloc2Defn ++
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
        (prologue ++ alloc2Defn ++
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
