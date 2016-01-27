module TestLLVM
    (alloc2,prologue,testCodeGen)
where

import Control.Monad(unless)
import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)
import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)

import Compile(compile)
import Parse(parse)
import Check(check)
import LowLevel(toLowLevel)
import LLVMCodeGen(codeGen)
import Runtime(annotateRuntime)

testCodeGen :: String -> String -> String -> Assertion
testCodeGen label srccode expected = do
    either (assertFailure . show) (assertEqual label expected)
           (compile (parse "(test)" srccode
                        >>= check
                        >>= fmap (codeGen . toLowLevel) . annotateRuntime))
    (exitCode,_,err) <-
        readProcessWithExitCode "llc" ["-filetype=null"] expected
    unless (exitCode == ExitSuccess) (assertFailure err)

prologue :: String
prologue = concat [
    "declare void @llvm.memset.p0i8.i8(i8*,i8,i8,i32,i1)",
    "define void @copy({i8,[0 x i1]}* %srcval,i8 %srcoffset,{i8,[0 x i1]}* %destval,i8 %destoffset,i8 %bitsize) { l0: br label %l1 l1: %0 = phi i8[0,%l0],[%7,%l2] %1 = icmp ult i8 %0,%bitsize br i1 %1, label %l2, label %l3 l2: %2 = add i8 %srcoffset,%0 %3 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %srcval,i32 0,i32 1,i8 %2 %4 = load i1,i1* %3 %5 = add i8 %destoffset,%0 %6 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %destval,i32 0,i32 1,i8 %5 store i1 %4,i1* %6 %7 = add i8 1,%0 br label %l1 l3: ret void }",
    "define void @copyrtt(i8** %srcval,i8 %srcoffset,i8** %destval,i8 %destoffset,i8 %size) { l0: br label %l1 l1: %0 = phi i8[0,%l0],[%7,%l2] %1 = icmp ult i8 %0,%size br i1 %1, label %l2, label %l3 l2: %2 = add i8 %srcoffset,%0 %3 = getelementptr i8*,i8** %srcval,i8 %2 %4 = load i8*,i8** %3 %5 = add i8 %destoffset,%0 %6 = getelementptr i8*,i8** %destval,i8 %5 store i8* %4,i8** %6 %7 = add i8 1,%0 br label %l1 l3: ret void }"
    ]

alloc2 :: String
alloc2 = "define {i8*,i8**} @alloc2({i8*,i8**} %a0,{i8*,i8**} %a1) { l0: br label %l1 l1: %0 = extractvalue {i8*,i8**} %a0,0 %1 = bitcast i8* %0 to {i8,[0 x i1]}* %2 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %1,i32 0,i32 0 %3 = load i8,i8* %2 %4 = icmp eq i8 0,%3 br i1 %4, label %l2, label %l3 l2: ret {i8*,i8**} %a0 l3: %5 = extractvalue {i8*,i8**} %a1,0 %6 = bitcast i8* %5 to {i8,[0 x i1]}* %7 = getelementptr {i8,[0 x i1]},{i8,[0 x i1]}* %6,i32 0,i32 0 %8 = load i8,i8* %7 %9 = icmp eq i8 0,%8 br i1 %9, label %l4, label %l5 l4: ret {i8*,i8**} %a1 l5: ret {i8*,i8**} undef }"
