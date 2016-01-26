module TestLLVMCodeGen
    (tests)
where

import Test.HUnit
    (Assertion,Test(..),assertEqual,assertFailure)

import Compile(compile)
import Parse(parse)
import Check(check)
import LowLevel(toLowLevel)
import LLVMCodeGen(codeGen)
import Runtime(annotateRuntime)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testInfiniteLoop
    ]

testCodeGen :: String -> String -> String -> Assertion
testCodeGen label srccode expected =
    either (assertFailure . show) (assertEqual label expected)
           (compile (parse "(test)" srccode
                        >>= check
                        >>= fmap (codeGen . toLowLevel) . annotateRuntime))

prologue :: String
prologue = concat [
    "declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)",
    "define void @copy({i8,[0 x i1]}* %srcval,i8 %srcoffset,{i8,[0 x i1]}* %destval,i8 %destoffset,i8 %bitsize) { l0: br label %l1 l1: %0 = phi i8[0,%l0],[%7,%l2] %1 = icmp ult i8 %0,%bitsize br i1 %1, label %l2, label %l3 l2: %2 = add i8 %srcoffset,%0 %3 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %srcval,i32 0,i32 1,i8 %2 %4 = load i1,i1* %3 %5 = add i8 %destoffset,%0 %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %destval,i32 0,i32 1,i8 %5 store i1 %4,i1* %6 %7 = add i8 1,%0 br label %l1 l3: ret void }",
    "define void @copyrtt(i8** %srcval,i8 %srcoffset,i8** %destval,i8 %destoffset,i8 %size) { l0: br label %l1 l1: %0 = phi i8[0,%l0],[%7,%l2] %1 = icmp ult i8 %0,%size br i1 %1, label %l2, label %l3 l2: %2 = add i8 %srcoffset,%0 %3 = getelementptr i8*,i8** %srcval,i8 %2 %4 = load i8*,i8** %3 %5 = add i8 %destoffset,%0 %6 = getelementptr i8*,i8** %destval,i8 %5 store i8* %4,i8** %6 %7 = add i8 1,%0 br label %l1 l3: ret void }"
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
            " call void @copy({i8,[0 x i1]} %6,i8 %7,{i8,[0 x i1]} %15,i8 0,i8 1)" ++
            " %16 = insertvalue {{i8,[0 x i1]}*,i8} undef,{i8,[0 x i1]}* %15,0" ++
            " %17 = insertvalue {{i8,[0 x i1]}*,i8} %16,i8 0,1" ++
            " ret {{i8,[0 x i1]}*,i8} %17" ++
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
