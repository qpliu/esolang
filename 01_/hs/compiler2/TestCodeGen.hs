module TestCodeGen
    (tests)
where

import Control.Monad(unless)
import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)
import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)

import Compile(compile)
import Parse(parse)
import Resolve(resolve)
import CodeGen(codeGen,genMain)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testFuncall,
    TestCase testConcat,
    TestCase testParam,
    TestCase testParam2,
    TestCase testParam3,
    TestCase testParam4,
    TestCase testParam5,
    TestCase testGenMain0,
    TestCase testGenMain1,
    TestCase testGenMain2,
    TestCase testGenMain3
    ]

testCodeGen :: String -> String -> String -> Assertion
testCodeGen label src expected = do
    either (assertFailure . show) (assertEqual label expected . codeGen)
           (compile (parse "(test)" src >>= resolve))
    (exitCode,_,err) <-
        readProcessWithExitCode "llc" ["-filetype=null"] expected
    unless (exitCode == ExitSuccess) (assertFailure err)

declares :: String
declares =
    ("declare i8* @malloc(i32)" ++
     "declare void @free(i8*)" ++
     "declare void @abort() noreturn " ++
     "declare i32 @read(i32,i8*,i32)" ++
     "declare i32 @write(i32,i8*,i32)" ++
     "declare i32 @open(i8*,i32)" ++
     "declare i32 @close(i32)" ++
     "declare void @perror(i8*)" ++
     "declare void @exit(i32) noreturn ")

defns :: String
defns =
    ("define private fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value) {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 0" ++
     " %1 = load i32,i32* %0" ++
     " %2 = sub i32 %1,1" ++
     " %3 = icmp ugt i32 %2,0" ++
     " br i1 %3,label %l1,label %l2" ++
     " l1: store i32 %2,i32* %0" ++
     " ret void" ++
     " l2:" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 1" ++
     " %5 = load i2,i2* %4 %6 = icmp eq i2 3,%5" ++
     " br i1 %6,label %l3,label %l4" ++
     " l3:" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 2" ++
     " %8 = load i8*,i8** %7" ++
     " %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 4" ++
     " %10 = load void(i8*)*,void(i8*)** %9" ++
     " call fastcc void %10(i8* %8)" ++
     " %11 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8*" ++
     " call void @free(i8* %11)" ++
     " ret void" ++
     " l4:" ++
     " %12 = icmp eq i2 2,%5" ++
     " br i1 %12,label %l5,label %l6" ++
     " l5:" ++
     " %13 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8*" ++
     " call void @free(i8* %13)" ++
     " ret void" ++
     " l6:" ++
     " %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 2" ++
     " %15 = load i8*,i8** %14" ++
     " %16 = bitcast i8* %15 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %17 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8*" ++
     " call void @free(i8* %17)" ++
     " musttail call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %16)" ++
     " ret void" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalLiteral(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {[0 x i1]*,i32,i32}*" ++
     " %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " %3 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 1" ++
     " %4 = load i32,i32* %3" ++
     " %5 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 2" ++
     " %6 = load i32,i32* %5" ++
     " %7 = icmp ult i32 %4,%6" ++
     " br i1 %7,label %l1,label %l2" ++
     " l1:" ++
     " %8 = add i32 1,%4" ++
     " store i32 %8,i32* %3" ++
     " %9 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 0" ++
     " %10 = load [0 x i1]*,[0 x i1]** %9" ++
     " %11 = getelementptr [0 x i1],[0 x i1]* %10,i32 0,i32 %4" ++
     " %12 = load i1,i1* %11" ++
     " %13 = zext i1 %12 to i2" ++
     " store i2 %13,i2* %2" ++
     " %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %15 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %14 to i32" ++
     " %16 = call i8* @malloc(i32 %15)" ++
     " %17 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 0" ++
     " store i32 1,i32* %18" ++
     " %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 1" ++
     " store i2 3,i2* %19" ++
     " %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %16,i8** %20" ++
     " %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 2" ++
     " store i8* %evalParam,i8** %21" ++
     " %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalLiteral,{i2,i8*}(i8*,i8*)** %22" ++
     " %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamLiteral,void(i8*)** %23" ++
     " %24 = insertvalue {i2,i8*} undef,i2 %13,0" ++
     " %25 = insertvalue {i2,i8*} %24,i8* %16,1" ++
     " ret {i2,i8*} %25" ++
     " l2:" ++
     " call void @free(i8* %evalParam)" ++
     " store i2 2,i2* %2" ++
     " %26 = insertvalue {i2,i8*} undef,i2 2,0" ++
     " ret {i2,i8*} %26" ++
     " }" ++
     "define private fastcc void @freeEvalParamLiteral(i8* %evalParam) {" ++
     " call void @free(i8* %evalParam)" ++
     " ret void" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalConcat(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}*" ++
     " %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %2 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 0" ++
     " %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2" ++
     -- %3 = first
     " %4 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 1" ++
     " %5 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %4" ++
     -- %5 = second
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " %7 = load i2,i2* %6" ++
     " %8 = icmp ne i2 3,%7" ++
     " br i1 %8,label %l1,label %l3" ++
     " l1:" ++
     " %9 = icmp ne i2 2,%7" ++
     " br i1 %9,label %l2,label %l4" ++
     " l2:" ++
     " %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " %11 = load i8*,i8** %10" ++
     " %12 = bitcast i8* %11 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     -- %12 = first.next
     " br label %l4" ++
     " l3:" ++
     " %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " %14 = load i8*,i8** %13" ++
     " %15 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " %16 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 to i8*" ++
     " %17 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %15" ++
     " %18 = call fastcc {i2,i8*} %17(i8* %14,i8* %16)" ++
     " %19 = extractvalue {i2,i8*} %18,0" ++
     " %20 = extractvalue {i2,i8*} %18,1" ++
     " %21 = bitcast i8* %20 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     -- %21 = first.next
     " br label %l4" ++
     " l4:" ++
     " %22 = phi i2 [%7,%l1],[%7,%l2],[%19,%l3]" ++
     " %23 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l1],[%12,%l2],[%21,%l3]" ++
     -- %23 = first.next
     " %24 = icmp eq i2 3,%22" ++
     " br i1 %24,label %l5,label %l6" ++
     " l5:" ++
     " call void @abort() noreturn" ++
     " ret {i2,i8*} undef" ++
     " l6:" ++
     " %25 = icmp ne i2 2,%22" ++
     " br i1 %25,label %l7,label %l8" ++
     " l7:" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 0" ++
     " %27 = load i32,i32* %26" ++
     " %28 = add i32 1,%27" ++
     " store i32 %28,i32* %26" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2" ++
     " %29 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %30 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29 to i32" ++
     " %31 = call i8* @malloc(i32 %30)" ++
     " %32 = bitcast i8* %31 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %33 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32,i32 0,i32 0" ++
     " store i32 1,i32* %33" ++
     " %34 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32,i32 0,i32 1" ++
     " store i2 3,i2* %34" ++
     " %35 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32,i32 0,i32 2" ++
     " store i8* %evalParam,i8** %35" ++
     " %36 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalConcat,{i2,i8*}(i8*,i8*)** %36" ++
     " %37 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamConcat,void(i8*)** %37" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 %22,i2* %38" ++
     " %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %31,i8** %39" ++
     " %40 = insertvalue {i2,i8*} undef,i2 %22,0" ++
     " %41 = insertvalue {i2,i8*} %40,i8* %31,1" ++
     " ret {i2,i8*} %41" ++
     " l8:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " call void @free(i8* %evalParam)" ++
     " %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 1" ++
     " %43 = load i2,i2* %42" ++
     " %44 = icmp eq i2 3,%43" ++
     " br i1 %44,label %l9,label %l12" ++
     " l9:" ++
     " %45 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5 to i8*" ++
     " %46 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2" ++
     " %47 = load i8*,i8** %46" ++
     " %48 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 3" ++
     " %49 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %48" ++
     " %50 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 0" ++
     " %51 = load i32,i32* %50" ++
     " %52 = icmp ule i32 %51,1" ++
     " br i1 %52,label %l10,label %l11" ++
     " l10:" ++
     " call void @free(i8* %45)" ++
     " %53 = musttail call fastcc {i2,i8*} %49(i8* %47,i8* %value)" ++
     " ret {i2,i8*} %53" ++
     " l11:" ++
     " %54 = call fastcc {i2,i8*} %49(i8* %47,i8* %value)" ++
     " %55 = extractvalue {i2,i8*} %54,0" ++
     " %56 = extractvalue {i2,i8*} %54,1" ++
     " br label %l13" ++
     " l12:" ++
     " %57 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2" ++
     " %58 = load i8*,i8** %57" ++
     " br label %l13" ++
     " l13:" ++
     " %59 = phi i2 [%55,%l11],[%43,%l12]" ++
     " %60 = phi i8* [%56,%l11],[%58,%l12]" ++
     -- %60 = second.next
     " %61 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 %59,i2* %61" ++
     " %62 = icmp eq i2 2,%59" ++
     " br i1 %62,label %l14,label %l15" ++
     " l14:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5)" ++
     " %63 = insertvalue {i2,i8*} undef,i2 %59,0" ++
     " ret {i2,i8*} %63" ++
     " l15:" ++
     " %64 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %60,i8** %64" ++
     " %65 = bitcast i8* %60 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %65,i32 0,i32 0" ++
     " %67 = load i32,i32* %66" ++
     " %68 = add i32 1,%67" ++
     " store i32 %68,i32* %66" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5)" ++
     " %69 = insertvalue {i2,i8*} undef,i2 %59,0" ++
     " %70 = insertvalue {i2,i8*} %69,i8* %60,1" ++
     " ret {i2,i8*} %70" ++
     " }" ++
     "define private fastcc void @freeEvalParamConcat(i8* %evalParam) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}*" ++
     " %1 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 0" ++
     " %2 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %1" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2)" ++
     " %3 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 1" ++
     " %4 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %3" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4)" ++
     " call void @free(i8* %evalParam)" ++
     " ret void" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFile(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {i32,i8,i8}*" ++
     " %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " %3 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 1" ++
     " %4 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 2" ++
     " %5 = load i8,i8* %3" ++
     " %6 = icmp slt i8 %5,0" ++
     " br i1 %6,label %l1,label %l2" ++
     " l1:" ++
     " %7 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 0" ++
     " %8 = load i32,i32* %7" ++
     " %9 = call i32 @read(i32 %8,i8* %4,i32 1)" ++
     " %10 = icmp eq i32 1,%9" ++
     " br i1 %10,label %l2,label %l3" ++
     " l2:" ++
     " %11 = phi i8 [%5,%l0],[7,%l1]" ++
     " %12 = sub i8 %11,1" ++
     " store i8 %12,i8* %3" ++
     " %13 = load i8,i8* %4" ++
     " %14 = lshr i8 %13,%11" ++
     " %15 = trunc i8 %14 to i2" ++
     " %16 = and i2 1,%15" ++
     " store i2 %16,i2* %2" ++
     " %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %18 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17 to i32" ++
     " %19 = call i8* @malloc(i32 %18)" ++
     " %20 = bitcast i8* %19 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 0" ++
     " store i32 1,i32* %21" ++
     " %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 1" ++
     " store i2 3,i2* %22" ++
     " %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %19,i8** %23" ++
     " %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 2" ++
     " store i8* %evalParam,i8** %24" ++
     " %25 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %25" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %26" ++
     " %27 = insertvalue {i2,i8*} undef,i2 %16,0" ++
     " %28 = insertvalue {i2,i8*} %27,i8* %19,1" ++
     " ret {i2,i8*} %28" ++
     " l3:" ++
     " %29 = call i32 @close(i32 %8)" ++
     " call void @free(i8* %evalParam)" ++
     " store i2 2,i2* %2" ++
     " %30 = insertvalue {i2,i8*} undef,i2 2,0" ++
     " ret {i2,i8*} %30" ++
     " }" ++
     "define private fastcc void @freeEvalParamFile(i8* %evalParam) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {i32,i8,i8}*" ++
     " %1 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 0" ++
     " %2 = load i32,i32* %1" ++
     " %3 = call i32 @close(i32 %2)" ++
     " call void @free(i8* %evalParam)" ++
     " ret void" ++
     " }" ++
     "define private fastcc void @freeEvalParamFunc(i8* %evalParam) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}*" ++
     " %1 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %0,i32 0,i32 0" ++
     " %2 = load i32,i32* %1" ++
     " br label %l1" ++
     " l1:" ++
     " %3 = phi i32 [0,%l0],[%7,%l2]" ++
     " %4 = icmp ult i32 %3,%2" ++
     " br i1 %4,label %l2,label %l3" ++
     " l2:" ++
     " %5 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %0,i32 0,i32 1,i32 %3" ++
     " %6 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %5" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6)" ++
     " %7 = add i32 1,%3" ++
     " br label %l1" ++
     " l3:" ++
     " call void @free(i8* %evalParam)" ++
     " ret void" ++
     " }" ++
     "define private fastcc void @freeEvalParamNullaryFunc(i8* %evalParam) {" ++
     " ret void" ++
     " }")

testSimple :: Assertion
testSimple = testCodeGen "testSimple" "f=."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f() {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 3,i2* %5" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %6" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamNullaryFunc,void(i8*)** %7" ++
     " ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l1" ++
     " l1:" ++
     " %1 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1" ++
     " store i2 2,i2* %1" ++
     " %2 = insertvalue {i2,i8*} undef,i2 2,0" ++
     " ret {i2,i8*} %2" ++
     " }")

testFuncall :: Assertion
testFuncall = testCodeGen "testFuncall" "f=f."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f() {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 3,i2* %5" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %6" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamNullaryFunc,void(i8*)** %7" ++
     " ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l1" ++
     " l1:" ++
     " %1 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f()" ++
     " %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " %3 = load i8*,i8** %2" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 3" ++
     " %5 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %4" ++
     " %6 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1 to i8*" ++
     " call void @free(i8* %6)" ++
     " %7 = musttail call fastcc {i2,i8*} %5(i8* %3,i8* %value)" ++
     " ret {i2,i8*} %7" ++
     " }")

testConcat :: Assertion
testConcat = testCodeGen "testConcat" "f=0f."
    ("@L0 = private constant [1 x i1] [i1 0]" ++
     declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f() {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 3,i2* %5" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %6" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamNullaryFunc,void(i8*)** %7" ++
     " ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l1" ++
     " l1:" ++
     " %1 = bitcast [1 x i1]* @L0 to [0 x i1]*" ++
     " %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %3 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2 to i32" ++
     " %4 = call i8* @malloc(i32 %3)" ++
     " %5 = bitcast i8* %4 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 0" ++
     " store i32 1,i32* %6" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 1" ++
     " store i2 3,i2* %7" ++
     " %8 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* null,i32 1" ++
     " %9 = ptrtoint {[0 x i1]*,i32,i32}* %8 to i32" ++
     " %10 = call i8* @malloc(i32 %9)" ++
     " %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2" ++
     " store i8* %10,i8** %11" ++
     " %12 = bitcast i8* %10 to {[0 x i1]*,i32,i32}*" ++
     " %13 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %12,i32 0,i32 0" ++
     " store [0 x i1]* %1,[0 x i1]** %13" ++
     " %14 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %12,i32 0,i32 1" ++
     " store i32 0,i32* %14" ++
     " %15 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %12,i32 0,i32 2" ++
     " store i32 1,i32* %15" ++
     " %16 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalLiteral,{i2,i8*}(i8*,i8*)** %16" ++
     " %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamLiteral,void(i8*)** %17" ++
     " %18 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f()" ++
     " %19 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* null,i32 1" ++
     " %20 = ptrtoint {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %19 to i32" ++
     " %21 = call i8* @malloc(i32 %20)" ++
     " %22 = bitcast i8* %21 to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}*" ++
     " %23 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %22,i32 0,i32 0" ++
     " store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %23" ++
     " %24 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %22,i32 0,i32 1" ++
     " store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %24" ++
     " %25 = musttail call fastcc {i2,i8*} @evalConcat(i8* %21,i8* %value)" ++
     " ret {i2,i8*} %25" ++
     " }")

testParam :: Assertion
testParam = testCodeGen "testParam" "f a=a."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0) {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 3,i2* %5" ++
     " %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 1" ++
     " %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32" ++
     " %8 = call i8* @malloc(i32 %7)" ++
     " %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}*" ++
     " %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0" ++
     " store i32 1,i32* %10" ++
     " %11 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 1,i32 0" ++
     " store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %11" ++
     " %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " store i8* %8,i8** %12" ++
     " %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %13" ++
     " %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFunc,void(i8*)** %14" ++
     " ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %1 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}*" ++
     " %2 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1,i32 0,i32 1,i32 0" ++
     " %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2" ++
     " call void @free(i8* %evalParam)" ++
     " br label %l1" ++
     " l1:" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %5 = load i32,i32* %4" ++
     " %6 = add i32 1,%5 store i32 %6,i32* %4" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " %8 = load i2,i2* %7" ++
     " %9 = icmp ne i2 3,%8" ++
     " br i1 %9,label %l2,label %l5" ++
     " l2:" ++
     " %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1" ++
     " store i2 %8,i2* %10" ++
     " %11 = icmp eq i2 2,%8" ++
     " br i1 %11,label %l3,label %l4" ++
     " l3:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " %12 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %12" ++
     " l4:" ++
     " %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " %14 = load i8*,i8** %13" ++
     " %15 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2" ++
     " store i8* %14,i8** %15" ++
     " %16 = bitcast i8* %14 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %16,i32 0,i32 0" ++
     " %18 = load i32,i32* %17" ++
     " %19 = add i32 1,%18 store i32 %19,i32* %17" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " %20 = insertvalue {i2,i8*} undef,i2 %8,0" ++
     " %21 = insertvalue {i2,i8*} %20,i8* %14,1" ++
     " ret {i2,i8*} %21" ++
     " l5:" ++
     " %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " %23 = load i8*,i8** %22" ++
     " %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " %25 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %24" ++
     " %26 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 to i8*" ++
     " %27 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %28 = load i32,i32* %27" ++
     " %29 = icmp ule i32 %28,1" ++
     " br i1 %29,label %l6,label %l7" ++
     " l6:" ++
     " call void @free(i8* %26)" ++
     " %30 = musttail call fastcc {i2,i8*} %25(i8* %23,i8* %value)" ++
     " ret {i2,i8*} %30" ++
     " l7:" ++
     " %31 = call fastcc {i2,i8*} %25(i8* %23,i8* %26)" ++
     " %32 = extractvalue {i2,i8*} %31,0" ++
     " %33 = extractvalue {i2,i8*} %31,1" ++
     " %34 = bitcast i8* %33 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %35 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %34,i32 0,i32 0" ++
     " %36 = load i32,i32* %35" ++
     " %37 = add i32 1,%36" ++
     " store i32 %37,i32* %35" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1" ++
     " store i2 %32,i2* %38" ++
     " %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2" ++
     " store i8* %33,i8** %39" ++
     " ret {i2,i8*} %31" ++
     " }")

testParam2 :: Assertion
testParam2 = testCodeGen "testParam2" "f 0a=a."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 3,i2* %5 %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 1,i32 0 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %11 %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 store i8* %8,i8** %12 %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %13 %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4 store void(i8*)* @freeEvalParamFunc,void(i8*)** %14 ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 }define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %2 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1,i32 0,i32 1,i32 0 %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2 call void @free(i8* %evalParam) br label %l1 l1: br label %l2 l2: %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %5 = load i2,i2* %4 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %7 = load i8*,i8** %6 %8 = icmp eq i2 3,%5 br i1 %8,label %l3,label %l4 l3: %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %10 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %9 %11 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %12 = call fastcc {i2,i8*} %10(i8* %7,i8* %3) %13 = extractvalue {i2,i8*} %12,0 %14 = extractvalue {i2,i8*} %12,1 br label %l4 l4: %15 = phi i2 [%5,%l2],[%13,%l3] %16 = phi i2 [%7,%l2],[%14,%l3] %17 = icmp eq i2 0,%15 br i1 %17,label %l5,label %l12 l5: %18 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 0 %20 = load i32,i32* %19 %21 = add i32 1,%20 store i32 %21,i32* %19 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 1 %23 = load i2,i2* %22 %24 = icmp ne i2 3,%23 br i1 %24,label %l6,label %l9 l6: %25 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %23,i2* %25 %26 = icmp eq i2 2,%23 br i1 %26,label %l7,label %l8 l7: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18) %27 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %27 l8: %28 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 2 %29 = load i8*,i8** %28 %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %29,i8** %30 %31 = bitcast i8* %29 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %32 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %31,i32 0,i32 0 %33 = load i32,i32* %32 %34 = add i32 1,%33 store i32 %34,i32* %32 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18) %35 = insertvalue {i2,i8*} undef,i2 %23,0 %36 = insertvalue {i2,i8*} %35,i8* %29,1 ret {i2,i8*} %36 l9: %37 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 2 %38 = load i8*,i8** %37 %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 3 %40 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %39 %41 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18 to i8* %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 0 %43 = load i32,i32* %42 %44 = icmp ule i32 %43,1 br i1 %44,label %l10,label %l11 l10: call void @free(i8* %41) %45 = musttail call fastcc {i2,i8*} %40(i8* %38,i8* %value) ret {i2,i8*} %45 l11: %46 = call fastcc {i2,i8*} %40(i8* %38,i8* %41) %47 = extractvalue {i2,i8*} %46,0 %48 = extractvalue {i2,i8*} %46,1 %49 = bitcast i8* %48 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %50 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %49,i32 0,i32 0 %51 = load i32,i32* %50 %52 = add i32 1,%51 store i32 %52,i32* %50 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18) %53 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %47,i2* %53 %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %48,i8** %54 ret {i2,i8*} %46 l12: call void @abort() noreturn ret {i2,i8*} undef }")

testParam3 :: Assertion
testParam3 = testCodeGen "testParam3" "f 0_=."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 3,i2* %5 %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 1,i32 0 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %11 %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 store i8* %8,i8** %12 %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %13 %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4 store void(i8*)* @freeEvalParamFunc,void(i8*)** %14 ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 }define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %2 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1,i32 0,i32 1,i32 0 %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2 call void @free(i8* %evalParam) br label %l1 l1: br label %l2 l2: %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %5 = load i2,i2* %4 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %7 = load i8*,i8** %6 %8 = icmp eq i2 3,%5 br i1 %8,label %l3,label %l4 l3: %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %10 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %9 %11 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %12 = call fastcc {i2,i8*} %10(i8* %7,i8* %3) %13 = extractvalue {i2,i8*} %12,0 %14 = extractvalue {i2,i8*} %12,1 br label %l4 l4: %15 = phi i2 [%5,%l2],[%13,%l3] %16 = phi i2 [%7,%l2],[%14,%l3] %17 = icmp eq i2 0,%15 br i1 %17,label %l5,label %l10 l5: %18 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l6 l6: %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 1 %20 = load i2,i2* %19 %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 2 %22 = load i8*,i8** %21 %23 = icmp eq i2 3,%20 br i1 %23,label %l7,label %l8 l7: %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 3 %25 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %24 %26 = bitcast i8* %18 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %27 = call fastcc {i2,i8*} %25(i8* %22,i8* %18) %28 = extractvalue {i2,i8*} %27,0 %29 = extractvalue {i2,i8*} %27,1 br label %l8 l8: %30 = phi i2 [%20,%l6],[%28,%l7] %31 = phi i2 [%22,%l6],[%29,%l7] %32 = icmp eq i2 2,%30 br i1 %32,label %l9,label %l10 l9: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %33 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 2,i2* %33 %34 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %34 l10: call void @abort() noreturn ret {i2,i8*} undef }")

testParam4 :: Assertion
testParam4 = testCodeGen "testParam4" "f 0.=."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 3,i2* %5 %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 1,i32 0 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %11 %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 store i8* %8,i8** %12 %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %13 %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4 store void(i8*)* @freeEvalParamFunc,void(i8*)** %14 ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 }define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %2 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1,i32 0,i32 1,i32 0 %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2 call void @free(i8* %evalParam) br label %l1 l1: br label %l2 l2: %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %5 = load i2,i2* %4 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %7 = load i8*,i8** %6 %8 = icmp eq i2 3,%5 br i1 %8,label %l3,label %l4 l3: %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %10 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %9 %11 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %12 = call fastcc {i2,i8*} %10(i8* %7,i8* %3) %13 = extractvalue {i2,i8*} %12,0 %14 = extractvalue {i2,i8*} %12,1 br label %l4 l4: %15 = phi i2 [%5,%l2],[%13,%l3] %16 = phi i2 [%7,%l2],[%14,%l3] %17 = icmp eq i2 0,%15 br i1 %17,label %l5,label %l6 l5: %18 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 2,i2* %19 %20 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %20 l6: call void @abort() noreturn ret {i2,i8*} undef }")

testParam5 :: Assertion
testParam5 = testCodeGen "testParam5" "f 00a=a.f 01a=a.f 10a=a.f a=."
    (declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 3,i2* %5 %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 1,i32 0 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %a0,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %11 %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 store i8* %8,i8** %12 %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %13 %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4 store void(i8*)* @freeEvalParamFunc,void(i8*)** %14 ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 }define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %2 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1,i32 0,i32 1,i32 0 %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2 call void @free(i8* %evalParam) br label %l1 l1: br label %l2 l2: %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %5 = load i2,i2* %4 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %7 = load i8*,i8** %6 %8 = icmp eq i2 3,%5 br i1 %8,label %l3,label %l4 l3: %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %10 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %9 %11 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %12 = call fastcc {i2,i8*} %10(i8* %7,i8* %3) %13 = extractvalue {i2,i8*} %12,0 %14 = extractvalue {i2,i8*} %12,1 br label %l4 l4: %15 = phi i2 [%5,%l2],[%13,%l3] %16 = phi i2 [%7,%l2],[%14,%l3] %17 = icmp eq i2 0,%15 br i1 %17,label %l5,label %l16 l5: %18 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l6 l6: %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 1 %20 = load i2,i2* %19 %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 2 %22 = load i8*,i8** %21 %23 = icmp eq i2 3,%20 br i1 %23,label %l7,label %l8 l7: %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18,i32 0,i32 3 %25 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %24 %26 = bitcast i8* %18 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %27 = call fastcc {i2,i8*} %25(i8* %22,i8* %18) %28 = extractvalue {i2,i8*} %27,0 %29 = extractvalue {i2,i8*} %27,1 br label %l8 l8: %30 = phi i2 [%20,%l6],[%28,%l7] %31 = phi i2 [%22,%l6],[%29,%l7] %32 = icmp eq i2 0,%30 br i1 %32,label %l9,label %l16 l9: %33 = bitcast i8* %31 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %34 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 0 %35 = load i32,i32* %34 %36 = add i32 1,%35 store i32 %36,i32* %34 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %37 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 1 %38 = load i2,i2* %37 %39 = icmp ne i2 3,%38 br i1 %39,label %l10,label %l13 l10: %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %38,i2* %40 %41 = icmp eq i2 2,%38 br i1 %41,label %l11,label %l12 l11: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33) %42 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %42 l12: %43 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 2 %44 = load i8*,i8** %43 %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %44,i8** %45 %46 = bitcast i8* %44 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %47 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %46,i32 0,i32 0 %48 = load i32,i32* %47 %49 = add i32 1,%48 store i32 %49,i32* %47 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33) %50 = insertvalue {i2,i8*} undef,i2 %38,0 %51 = insertvalue {i2,i8*} %50,i8* %44,1 ret {i2,i8*} %51 l13: %52 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 2 %53 = load i8*,i8** %52 %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 3 %55 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %54 %56 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33 to i8* %57 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33,i32 0,i32 0 %58 = load i32,i32* %57 %59 = icmp ule i32 %58,1 br i1 %59,label %l14,label %l15 l14: call void @free(i8* %56) %60 = musttail call fastcc {i2,i8*} %55(i8* %53,i8* %value) ret {i2,i8*} %60 l15: %61 = call fastcc {i2,i8*} %55(i8* %53,i8* %56) %62 = extractvalue {i2,i8*} %61,0 %63 = extractvalue {i2,i8*} %61,1 %64 = bitcast i8* %63 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %65 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %64,i32 0,i32 0 %66 = load i32,i32* %65 %67 = add i32 1,%66 store i32 %67,i32* %65 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %33) %68 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %62,i2* %68 %69 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %63,i8** %69 ret {i2,i8*} %61 l16: br label %l17 l17: %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %71 = load i2,i2* %70 %72 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %73 = load i8*,i8** %72 %74 = icmp eq i2 3,%71 br i1 %74,label %l18,label %l19 l18: %75 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %76 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %75 %77 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %78 = call fastcc {i2,i8*} %76(i8* %73,i8* %3) %79 = extractvalue {i2,i8*} %78,0 %80 = extractvalue {i2,i8*} %78,1 br label %l19 l19: %81 = phi i2 [%71,%l17],[%79,%l18] %82 = phi i2 [%73,%l17],[%80,%l18] %83 = icmp eq i2 0,%81 br i1 %83,label %l20,label %l31 l20: %84 = bitcast i8* %82 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l21 l21: %85 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %84,i32 0,i32 1 %86 = load i2,i2* %85 %87 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %84,i32 0,i32 2 %88 = load i8*,i8** %87 %89 = icmp eq i2 3,%86 br i1 %89,label %l22,label %l23 l22: %90 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %84,i32 0,i32 3 %91 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %90 %92 = bitcast i8* %84 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %93 = call fastcc {i2,i8*} %91(i8* %88,i8* %84) %94 = extractvalue {i2,i8*} %93,0 %95 = extractvalue {i2,i8*} %93,1 br label %l23 l23: %96 = phi i2 [%86,%l21],[%94,%l22] %97 = phi i2 [%88,%l21],[%95,%l22] %98 = icmp eq i2 1,%96 br i1 %98,label %l24,label %l31 l24: %99 = bitcast i8* %97 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %100 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 0 %101 = load i32,i32* %100 %102 = add i32 1,%101 store i32 %102,i32* %100 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %103 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 1 %104 = load i2,i2* %103 %105 = icmp ne i2 3,%104 br i1 %105,label %l25,label %l28 l25: %106 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %104,i2* %106 %107 = icmp eq i2 2,%104 br i1 %107,label %l26,label %l27 l26: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99) %108 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %108 l27: %109 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 2 %110 = load i8*,i8** %109 %111 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %110,i8** %111 %112 = bitcast i8* %110 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %113 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %112,i32 0,i32 0 %114 = load i32,i32* %113 %115 = add i32 1,%114 store i32 %115,i32* %113 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99) %116 = insertvalue {i2,i8*} undef,i2 %104,0 %117 = insertvalue {i2,i8*} %116,i8* %110,1 ret {i2,i8*} %117 l28: %118 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 2 %119 = load i8*,i8** %118 %120 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 3 %121 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %120 %122 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99 to i8* %123 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 0 %124 = load i32,i32* %123 %125 = icmp ule i32 %124,1 br i1 %125,label %l29,label %l30 l29: call void @free(i8* %122) %126 = musttail call fastcc {i2,i8*} %121(i8* %119,i8* %value) ret {i2,i8*} %126 l30: %127 = call fastcc {i2,i8*} %121(i8* %119,i8* %122) %128 = extractvalue {i2,i8*} %127,0 %129 = extractvalue {i2,i8*} %127,1 %130 = bitcast i8* %129 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %131 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %130,i32 0,i32 0 %132 = load i32,i32* %131 %133 = add i32 1,%132 store i32 %133,i32* %131 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99) %134 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %128,i2* %134 %135 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %129,i8** %135 ret {i2,i8*} %127 l31: br label %l32 l32: %136 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 %137 = load i2,i2* %136 %138 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %139 = load i8*,i8** %138 %140 = icmp eq i2 3,%137 br i1 %140,label %l33,label %l34 l33: %141 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %142 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %141 %143 = bitcast i8* %3 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %144 = call fastcc {i2,i8*} %142(i8* %139,i8* %3) %145 = extractvalue {i2,i8*} %144,0 %146 = extractvalue {i2,i8*} %144,1 br label %l34 l34: %147 = phi i2 [%137,%l32],[%145,%l33] %148 = phi i2 [%139,%l32],[%146,%l33] %149 = icmp eq i2 1,%147 br i1 %149,label %l35,label %l46 l35: %150 = bitcast i8* %148 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l36 l36: %151 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %150,i32 0,i32 1 %152 = load i2,i2* %151 %153 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %150,i32 0,i32 2 %154 = load i8*,i8** %153 %155 = icmp eq i2 3,%152 br i1 %155,label %l37,label %l38 l37: %156 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %150,i32 0,i32 3 %157 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %156 %158 = bitcast i8* %150 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %159 = call fastcc {i2,i8*} %157(i8* %154,i8* %150) %160 = extractvalue {i2,i8*} %159,0 %161 = extractvalue {i2,i8*} %159,1 br label %l38 l38: %162 = phi i2 [%152,%l36],[%160,%l37] %163 = phi i2 [%154,%l36],[%161,%l37] %164 = icmp eq i2 0,%162 br i1 %164,label %l39,label %l46 l39: %165 = bitcast i8* %163 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %166 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 0 %167 = load i32,i32* %166 %168 = add i32 1,%167 store i32 %168,i32* %166 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %169 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 1 %170 = load i2,i2* %169 %171 = icmp ne i2 3,%170 br i1 %171,label %l40,label %l43 l40: %172 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %170,i2* %172 %173 = icmp eq i2 2,%170 br i1 %173,label %l41,label %l42 l41: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165) %174 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %174 l42: %175 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 2 %176 = load i8*,i8** %175 %177 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %176,i8** %177 %178 = bitcast i8* %176 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %179 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %178,i32 0,i32 0 %180 = load i32,i32* %179 %181 = add i32 1,%180 store i32 %181,i32* %179 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165) %182 = insertvalue {i2,i8*} undef,i2 %170,0 %183 = insertvalue {i2,i8*} %182,i8* %176,1 ret {i2,i8*} %183 l43: %184 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 2 %185 = load i8*,i8** %184 %186 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 3 %187 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %186 %188 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165 to i8* %189 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165,i32 0,i32 0 %190 = load i32,i32* %189 %191 = icmp ule i32 %190,1 br i1 %191,label %l44,label %l45 l44: call void @free(i8* %188) %192 = musttail call fastcc {i2,i8*} %187(i8* %185,i8* %value) ret {i2,i8*} %192 l45: %193 = call fastcc {i2,i8*} %187(i8* %185,i8* %188) %194 = extractvalue {i2,i8*} %193,0 %195 = extractvalue {i2,i8*} %193,1 %196 = bitcast i8* %195 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %197 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %196,i32 0,i32 0 %198 = load i32,i32* %197 %199 = add i32 1,%198 store i32 %199,i32* %197 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %165) %200 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 %194,i2* %200 %201 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 2 store i8* %195,i8** %201 ret {i2,i8*} %193 l46: %202 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %203 = load i32,i32* %202 %204 = add i32 1,%203 store i32 %204,i32* %202 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %205 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0,i32 0,i32 1 store i2 2,i2* %205 %206 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %206 }")

testGenMain :: String -> String -> String -> Assertion
testGenMain label src expected = do
    funcs@((name,defs):_) <-
        either (undefined . assertFailure . show) return
               (compile (parse "(test)" src >>= resolve))
    assertEqual label expected (genMain name defs)
    (exitCode,_,err) <-
        readProcessWithExitCode "llc" ["-filetype=null"]
                                (codeGen funcs ++ expected)
    unless (exitCode == ExitSuccess) (assertFailure err)

testGenMain0 :: Assertion
testGenMain0 = testGenMain "testGenMain0" "f=."
    ("define void @main(i32 %argc,i8** %argv) {" ++
     " l0:" ++
     " %0 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f()" ++
     " br label %l1" ++
     " l1:" ++
     " %1 = alloca i8,i32 1" ++
     " br label %l2" ++
     " l2:" ++
     " %2 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%0,%l1],[%22,%l11],[%22,%l10]" ++
     " %3 = phi i8 [7,%l1],[%31,%l10],[7,%l11]" ++
     " %4 = phi i8 [0,%l1],[%30,%l10],[0,%l11]" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2,i32 0,i32 1" ++
     " %6 = load i2,i2* %5" ++
     " %7 = icmp ne i2 3,%6" ++
     " br i1 %7,label %l3,label %l5" ++
     " l3:" ++
     " %8 = icmp ne i2 2,%6" ++
     " br i1 %8,label %l4,label %l6" ++
     " l4:" ++
     " %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2,i32 0,i32 2" ++
     " %10 = load i8*,i8** %9" ++
     " %11 = bitcast i8* %10 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l6" ++
     " l5:" ++
     " %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2,i32 0,i32 2" ++
     " %13 = load i8*,i8** %12" ++
     " %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2,i32 0,i32 3" ++
     " %15 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2 to i8*" ++
     " %16 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %14" ++
     " %17 = call fastcc {i2,i8*} %16(i8* %13,i8* %15)" ++
     " %18 = extractvalue {i2,i8*} %17,0" ++
     " %19 = extractvalue {i2,i8*} %17,1" ++
     " %20 = bitcast i8* %19 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l6" ++
     " l6:" ++
     " %21 = phi i2 [%6,%l3],[%6,%l4],[%18,%l5]" ++
     " %22 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l3],[%11,%l4],[%20,%l5]" ++
     " %23 = icmp eq i2 3,%21" ++
     " br i1 %23,label %l7,label %l8" ++
     " l7:" ++
     " call void @abort() noreturn" ++
     " ret void" ++
     " l8:" ++
     " %24 = icmp eq i2 2,%21" ++
     " br i1 %24,label %l9,label %l10" ++
     " l9:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2)" ++
     " call void @exit(i32 0) noreturn" ++
     " ret void" ++
     " l10:" ++
     " %25 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %22,i32 0,i32 0" ++
     " %26 = load i32,i32* %25" ++
     " %27 = add i32 1,%26" ++
     " store i32 %27,i32* %25" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2)" ++
     " %28 = zext i2 %21 to i8" ++
     " %29 = shl i8 %28,%3" ++
     " %30 = or i8 %4,%29" ++
     " %31 = sub i8 %3,1" ++
     " %32 = icmp sge i8 %31,0" ++
     " br i1 %32,label %l2,label %l11" ++
     " l11:" ++
     " store i8 %30,i8* %1" ++
     " %33 = call i32 @write(i32 1,i8* %1,i32 1)" ++
     " br label %l2" ++
     " }")

testGenMain1 :: Assertion
testGenMain1 = testGenMain "testGenMain1" "f a=."
    ("define void @main(i32 %argc,i8** %argv) {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 2,i2* %5" ++
     -- %3 = nil
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32" ++
     " %8 = call i8* @malloc(i32 %7)" ++
     " %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " store i32 1,i32* %10" ++
     " %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1" ++
     " store i2 3,i2* %11" ++
     " %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %13 = ptrtoint {i32,i8,i8}* %12 to i32" ++
     " %14 = call i8* @malloc(i32 %13)" ++
     " %15 = bitcast i8* %14 to {i32,i8,i8}*" ++
     " %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0" ++
     " store i32 0,i32* %16" ++
     " %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1" ++
     " store i8 -1,i8* %17" ++
     " %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2" ++
     " store i8* %14,i8** %18" ++
     " %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19" ++
     " %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %20" ++
     -- %9 = stdin
     " %21 = icmp ult i32 1,%argc" ++
     " br i1 %21,label %l1,label %l4" ++
     " l1:" ++
     " %22 = getelementptr i8*,i8** %argv,i32 1" ++
     " %23 = load i8*,i8** %22" ++
     " %24 = call i32 @open(i8* %23,i32 0)" ++
     " %25 = icmp sge i32 %24,0" ++
     " br i1 %25,label %l2,label %l3" ++
     " l2:" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32" ++
     " %28 = call i8* @malloc(i32 %27)" ++
     " %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0" ++
     " store i32 1,i32* %30" ++
     " %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1" ++
     " store i2 3,i2* %31" ++
     " %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %33 = ptrtoint {i32,i8,i8}* %32 to i32" ++
     " %34 = call i8* @malloc(i32 %33)" ++
     " %35 = bitcast i8* %34 to {i32,i8,i8}*" ++
     " %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0" ++
     " store i32 %24,i32* %36" ++
     " %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1" ++
     " store i8 -1,i8* %37" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2" ++
     " store i8* %34,i8** %38" ++
     " %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39" ++
     " %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %40" ++
     " br label %l7" ++
     " l3:" ++
     " call void @perror(i8* %23)" ++
     " call void @exit(i32 -1) noreturn" ++
     " ret void" ++
     " l4:" ++
     " %41 = icmp eq i32 1,%argc" ++
     " br i1 %41,label %l5,label %l6" ++
     " l5:" ++
     " %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %43 = load i32,i32* %42" ++
     " %44 = add i32 1,%43" ++
     " store i32 %44,i32* %42" ++
     " br label %l7" ++
     " l6:" ++
     " %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %46 = load i32,i32* %45" ++
     " %47 = add i32 1,%46" ++
     " store i32 %47,i32* %45" ++
     " br label %l7" ++
     " l7:" ++
     " %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6]" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9)" ++
     " %49 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48)" ++
     " br label %l8" ++
     " l8:" ++
     " %50 = alloca i8,i32 1" ++
     " br label %l9" ++
     " l9:" ++
     " %51 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%49,%l8],[%71,%l18],[%71,%l17]" ++
     " %52 = phi i8 [7,%l8],[%80,%l17],[7,%l18]" ++
     " %53 = phi i8 [0,%l8],[%79,%l17],[0,%l18]" ++
     " %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 1" ++
     " %55 = load i2,i2* %54" ++
     " %56 = icmp ne i2 3,%55" ++
     " br i1 %56,label %l10,label %l12" ++
     " l10:" ++
     " %57 = icmp ne i2 2,%55" ++
     " br i1 %57,label %l11,label %l13" ++
     " l11:" ++
     " %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 2" ++
     " %59 = load i8*,i8** %58" ++
     " %60 = bitcast i8* %59 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l13" ++
     " l12:" ++
     " %61 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 2" ++
     " %62 = load i8*,i8** %61" ++
     " %63 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 3" ++
     " %64 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51 to i8*" ++
     " %65 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %63" ++
     " %66 = call fastcc {i2,i8*} %65(i8* %62,i8* %64)" ++
     " %67 = extractvalue {i2,i8*} %66,0" ++
     " %68 = extractvalue {i2,i8*} %66,1" ++
     " %69 = bitcast i8* %68 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l13" ++
     " l13:" ++
     " %70 = phi i2 [%55,%l10],[%55,%l11],[%67,%l12]" ++
     " %71 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l10],[%60,%l11],[%69,%l12]" ++
     " %72 = icmp eq i2 3,%70" ++
     " br i1 %72,label %l14,label %l15" ++
     " l14:" ++
     " call void @abort() noreturn" ++
     " ret void" ++
     " l15:" ++
     " %73 = icmp eq i2 2,%70" ++
     " br i1 %73,label %l16,label %l17" ++
     " l16:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51)" ++
     " call void @exit(i32 0) noreturn" ++
     " ret void" ++
     " l17:" ++
     " %74 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %71,i32 0,i32 0" ++
     " %75 = load i32,i32* %74" ++
     " %76 = add i32 1,%75" ++
     " store i32 %76,i32* %74" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51)" ++
     " %77 = zext i2 %70 to i8" ++
     " %78 = shl i8 %77,%52" ++
     " %79 = or i8 %53,%78" ++
     " %80 = sub i8 %52,1" ++
     " %81 = icmp sge i8 %80,0" ++
     " br i1 %81,label %l9,label %l18" ++
     " l18:" ++
     " store i8 %79,i8* %50" ++
     " %82 = call i32 @write(i32 1,i8* %50,i32 1)" ++
     " br label %l9" ++
     " }")

testGenMain2 :: Assertion
testGenMain2 = testGenMain "testGenMain2" "f a b=."
    ("define void @main(i32 %argc,i8** %argv) {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 2,i2* %5" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32" ++
     " %8 = call i8* @malloc(i32 %7)" ++
     " %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " store i32 1,i32* %10" ++
     " %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1" ++
     " store i2 3,i2* %11" ++
     " %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %13 = ptrtoint {i32,i8,i8}* %12 to i32" ++
     " %14 = call i8* @malloc(i32 %13)" ++
     " %15 = bitcast i8* %14 to {i32,i8,i8}*" ++
     " %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0" ++
     " store i32 0,i32* %16" ++
     " %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1" ++
     " store i8 -1,i8* %17" ++
     " %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2" ++
     " store i8* %14,i8** %18" ++
     " %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19" ++
     " %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %20" ++
     " %21 = icmp ult i32 1,%argc" ++
     " br i1 %21,label %l1,label %l4" ++
     " l1:" ++
     " %22 = getelementptr i8*,i8** %argv,i32 1" ++
     " %23 = load i8*,i8** %22" ++
     " %24 = call i32 @open(i8* %23,i32 0)" ++
     " %25 = icmp sge i32 %24,0" ++
     " br i1 %25,label %l2,label %l3" ++
     " l2:" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32" ++
     " %28 = call i8* @malloc(i32 %27)" ++
     " %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0" ++
     " store i32 1,i32* %30" ++
     " %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1" ++
     " store i2 3,i2* %31" ++
     " %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %33 = ptrtoint {i32,i8,i8}* %32 to i32" ++
     " %34 = call i8* @malloc(i32 %33)" ++
     " %35 = bitcast i8* %34 to {i32,i8,i8}*" ++
     " %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0" ++
     " store i32 %24,i32* %36" ++
     " %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1" ++
     " store i8 -1,i8* %37" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2" ++
     " store i8* %34,i8** %38" ++
     " %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39" ++
     " %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %40" ++
     " br label %l7" ++
     " l3:" ++
     " call void @perror(i8* %23)" ++
     " call void @exit(i32 -1)" ++
     " noreturn ret void" ++
     " l4:" ++
     " %41 = icmp eq i32 1,%argc" ++
     " br i1 %41,label %l5,label %l6" ++
     " l5:" ++
     " %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %43 = load i32,i32* %42" ++
     " %44 = add i32 1,%43" ++
     " store i32 %44,i32* %42" ++
     " br label %l7" ++
     " l6:" ++
     " %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %46 = load i32,i32* %45" ++
     " %47 = add i32 1,%46" ++
     " store i32 %47,i32* %45" ++
     " br label %l7" ++
     " l7:" ++
     " %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6]" ++
     " %49 = icmp ult i32 2,%argc" ++
     " br i1 %49,label %l8,label %l11" ++
     " l8:" ++
     " %50 = getelementptr i8*,i8** %argv,i32 2" ++
     " %51 = load i8*,i8** %50" ++
     " %52 = call i32 @open(i8* %51,i32 0)" ++
     " %53 = icmp sge i32 %52,0" ++
     " br i1 %53,label %l9,label %l10" ++
     " l9:" ++
     " %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %55 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %54 to i32" ++
     " %56 = call i8* @malloc(i32 %55)" ++
     " %57 = bitcast i8* %56 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 0" ++
     " store i32 1,i32* %58" ++
     " %59 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 1" ++
     " store i2 3,i2* %59" ++
     " %60 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %61 = ptrtoint {i32,i8,i8}* %60 to i32" ++
     " %62 = call i8* @malloc(i32 %61)" ++
     " %63 = bitcast i8* %62 to {i32,i8,i8}*" ++
     " %64 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 0" ++
     " store i32 %52,i32* %64" ++
     " %65 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 1" ++
     " store i8 -1,i8* %65" ++
     " %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 2" ++
     " store i8* %62,i8** %66" ++
     " %67 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %67" ++
     " %68 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %68" ++
     " br label %l14" ++
     " l10:" ++
     " call void @perror(i8* %51)" ++
     " call void @exit(i32 -1) noreturn" ++
     " ret void" ++
     " l11:" ++
     " %69 = icmp eq i32 2,%argc" ++
     " br i1 %69,label %l12,label %l13" ++
     " l12:" ++
     " %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %71 = load i32,i32* %70" ++
     " %72 = add i32 1,%71" ++
     " store i32 %72,i32* %70" ++
     " br label %l14" ++
     " l13:" ++
     " %73 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %74 = load i32,i32* %73" ++
     " %75 = add i32 1,%74" ++
     " store i32 %75,i32* %73" ++
     " br label %l14" ++
     " l14:" ++
     " %76 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%57,%l9],[%9,%l12],[%3,%l13]" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9)" ++
     " %77 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %76)" ++
     " br label %l15" ++
     " l15:" ++
     " %78 = alloca i8,i32 1" ++
     " br label %l16" ++
     " l16:" ++
     " %79 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%77,%l15],[%99,%l25],[%99,%l24]" ++
     " %80 = phi i8 [7,%l15],[%108,%l24],[7,%l25]" ++
     " %81 = phi i8 [0,%l15],[%107,%l24],[0,%l25]" ++
     " %82 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 1" ++
     " %83 = load i2,i2* %82" ++
     " %84 = icmp ne i2 3,%83" ++
     " br i1 %84,label %l17,label %l19" ++
     " l17:" ++
     " %85 = icmp ne i2 2,%83" ++
     " br i1 %85,label %l18,label %l20" ++
     " l18:" ++
     " %86 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 2" ++
     " %87 = load i8*,i8** %86" ++
     " %88 = bitcast i8* %87 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l20" ++
     " l19:" ++
     " %89 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 2" ++
     " %90 = load i8*,i8** %89" ++
     " %91 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 3" ++
     " %92 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79 to i8*" ++
     " %93 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %91" ++
     " %94 = call fastcc {i2,i8*} %93(i8* %90,i8* %92)" ++
     " %95 = extractvalue {i2,i8*} %94,0" ++
     " %96 = extractvalue {i2,i8*} %94,1" ++
     " %97 = bitcast i8* %96 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l20" ++
     " l20:" ++
     " %98 = phi i2 [%83,%l17],[%83,%l18],[%95,%l19]" ++
     " %99 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l17],[%88,%l18],[%97,%l19]" ++
     " %100 = icmp eq i2 3,%98" ++
     " br i1 %100,label %l21,label %l22" ++
     " l21:" ++
     " call void @abort() noreturn" ++
     " ret void" ++
     " l22:" ++
     " %101 = icmp eq i2 2,%98" ++
     " br i1 %101,label %l23,label %l24" ++
     " l23:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79)" ++
     " call void @exit(i32 0) noreturn" ++
     " ret void" ++
     " l24:" ++
     " %102 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 0" ++
     " %103 = load i32,i32* %102" ++
     " %104 = add i32 1,%103" ++
     " store i32 %104,i32* %102" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79)" ++
     " %105 = zext i2 %98 to i8" ++
     " %106 = shl i8 %105,%80" ++
     " %107 = or i8 %81,%106" ++
     " %108 = sub i8 %80,1" ++
     " %109 = icmp sge i8 %108,0" ++
     " br i1 %109,label %l16,label %l25" ++
     " l25:" ++
     " store i8 %107,i8* %78" ++
     " %110 = call i32 @write(i32 1,i8* %78,i32 1)" ++
     " br label %l16" ++
     " }")

testGenMain3 :: Assertion
testGenMain3 = testGenMain "testGenMain3" "f a b c=."
    ("define void @main(i32 %argc,i8** %argv) {" ++
     " l0:" ++
     " %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32" ++
     " %2 = call i8* @malloc(i32 %1)" ++
     " %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " store i32 1,i32* %4" ++
     " %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1" ++
     " store i2 2,i2* %5" ++
     " %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32" ++
     " %8 = call i8* @malloc(i32 %7)" ++
     " %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " store i32 1,i32* %10" ++
     " %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1" ++
     " store i2 3,i2* %11" ++
     " %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %13 = ptrtoint {i32,i8,i8}* %12 to i32" ++
     " %14 = call i8* @malloc(i32 %13)" ++
     " %15 = bitcast i8* %14 to {i32,i8,i8}*" ++
     " %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0" ++
     " store i32 0,i32* %16" ++
     " %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1" ++
     " store i8 -1,i8* %17" ++
     " %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2" ++
     " store i8* %14,i8** %18" ++
     " %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19" ++
     " %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %20" ++
     " %21 = icmp ult i32 1,%argc" ++
     " br i1 %21,label %l1,label %l4" ++
     " l1:" ++
     " %22 = getelementptr i8*,i8** %argv,i32 1" ++
     " %23 = load i8*,i8** %22" ++
     " %24 = call i32 @open(i8* %23,i32 0)" ++
     " %25 = icmp sge i32 %24,0" ++
     " br i1 %25,label %l2,label %l3" ++
     " l2:" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32" ++
     " %28 = call i8* @malloc(i32 %27)" ++
     " %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0" ++
     " store i32 1,i32* %30" ++
     " %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1" ++
     " store i2 3,i2* %31" ++
     " %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %33 = ptrtoint {i32,i8,i8}* %32 to i32" ++
     " %34 = call i8* @malloc(i32 %33)" ++
     " %35 = bitcast i8* %34 to {i32,i8,i8}*" ++
     " %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0" ++
     " store i32 %24,i32* %36" ++
     " %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1" ++
     " store i8 -1,i8* %37" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2" ++
     " store i8* %34,i8** %38" ++
     " %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39" ++
     " %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %40" ++
     " br label %l7" ++
     " l3:" ++
     " call void @perror(i8* %23)" ++
     " call void @exit(i32 -1) noreturn" ++
     " ret void" ++
     " l4:" ++
     " %41 = icmp eq i32 1,%argc" ++
     " br i1 %41,label %l5,label %l6" ++
     " l5:" ++
     " %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %43 = load i32,i32* %42" ++
     " %44 = add i32 1,%43" ++
     " store i32 %44,i32* %42" ++
     " br label %l7" ++
     " l6:" ++
     " %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %46 = load i32,i32* %45" ++
     " %47 = add i32 1,%46" ++
     " store i32 %47,i32* %45" ++
     " br label %l7" ++
     " l7:" ++
     " %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6]" ++
     " %49 = icmp ult i32 2,%argc" ++
     " br i1 %49,label %l8,label %l11" ++
     " l8:" ++
     " %50 = getelementptr i8*,i8** %argv,i32 2" ++
     " %51 = load i8*,i8** %50" ++
     " %52 = call i32 @open(i8* %51,i32 0)" ++
     " %53 = icmp sge i32 %52,0" ++
     " br i1 %53,label %l9,label %l10" ++
     " l9:" ++
     " %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %55 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %54 to i32" ++
     " %56 = call i8* @malloc(i32 %55)" ++
     " %57 = bitcast i8* %56 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 0" ++
     " store i32 1,i32* %58" ++
     " %59 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 1" ++
     " store i2 3,i2* %59" ++
     " %60 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %61 = ptrtoint {i32,i8,i8}* %60 to i32" ++
     " %62 = call i8* @malloc(i32 %61)" ++
     " %63 = bitcast i8* %62 to {i32,i8,i8}*" ++
     " %64 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 0" ++
     " store i32 %52,i32* %64" ++
     " %65 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 1" ++
     " store i8 -1,i8* %65" ++
     " %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 2" ++
     " store i8* %62,i8** %66" ++
     " %67 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %67" ++
     " %68 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %68" ++
     " br label %l14" ++
     " l10:" ++
     " call void @perror(i8* %51)" ++
     " call void @exit(i32 -1) noreturn" ++
     " ret void" ++
     " l11:" ++
     " %69 = icmp eq i32 2,%argc" ++
     " br i1 %69,label %l12,label %l13" ++
     " l12:" ++
     " %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %71 = load i32,i32* %70" ++
     " %72 = add i32 1,%71" ++
     " store i32 %72,i32* %70" ++
     " br label %l14" ++
     " l13:" ++
     " %73 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %74 = load i32,i32* %73" ++
     " %75 = add i32 1,%74" ++
     " store i32 %75,i32* %73" ++
     " br label %l14" ++
     " l14:" ++
     " %76 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%57,%l9],[%9,%l12],[%3,%l13]" ++
     " %77 = icmp ult i32 3,%argc" ++
     " br i1 %77,label %l15,label %l18" ++
     " l15:" ++
     " %78 = getelementptr i8*,i8** %argv,i32 3" ++
     " %79 = load i8*,i8** %78" ++
     " %80 = call i32 @open(i8* %79,i32 0)" ++
     " %81 = icmp sge i32 %80,0" ++
     " br i1 %81,label %l16,label %l17" ++
     " l16:" ++
     " %82 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %83 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %82 to i32" ++
     " %84 = call i8* @malloc(i32 %83)" ++
     " %85 = bitcast i8* %84 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %86 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 0" ++
     " store i32 1,i32* %86" ++
     " %87 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 1" ++
     " store i2 3,i2* %87" ++
     " %88 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1" ++
     " %89 = ptrtoint {i32,i8,i8}* %88 to i32" ++
     " %90 = call i8* @malloc(i32 %89)" ++
     " %91 = bitcast i8* %90 to {i32,i8,i8}*" ++
     " %92 = getelementptr {i32,i8,i8},{i32,i8,i8}* %91,i32 0,i32 0" ++
     " store i32 %80,i32* %92" ++
     " %93 = getelementptr {i32,i8,i8},{i32,i8,i8}* %91,i32 0,i32 1" ++
     " store i8 -1,i8* %93" ++
     " %94 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 2" ++
     " store i8* %90,i8** %94" ++
     " %95 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %95" ++
     " %96 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFile,void(i8*)** %96" ++
     " br label %l21" ++
     " l17:" ++
     " call void @perror(i8* %79)" ++
     " call void @exit(i32 -1) noreturn" ++
     " ret void" ++
     " l18:" ++
     " %97 = icmp eq i32 3,%argc" ++
     " br i1 %97,label %l19,label %l20" ++
     " l19:" ++
     " %98 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0" ++
     " %99 = load i32,i32* %98" ++
     " %100 = add i32 1,%99" ++
     " store i32 %100,i32* %98" ++
     " br label %l21" ++
     " l20:" ++
     " %101 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0" ++
     " %102 = load i32,i32* %101" ++
     " %103 = add i32 1,%102" ++
     " store i32 %103,i32* %101" ++
     " br label %l21" ++
     " l21:" ++
     " %104 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%85,%l16],[%9,%l19],[%3,%l20]" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3)" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9)" ++
     " %105 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %76,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %104)" ++
     " br label %l22" ++
     " l22:" ++
     " %106 = alloca i8,i32 1" ++
     " br label %l23" ++
     " l23:" ++
     " %107 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%105,%l22],[%127,%l32],[%127,%l31]" ++
     " %108 = phi i8 [7,%l22],[%136,%l31],[7,%l32]" ++
     " %109 = phi i8 [0,%l22],[%135,%l31],[0,%l32]" ++
     " %110 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 1" ++
     " %111 = load i2,i2* %110" ++
     " %112 = icmp ne i2 3,%111" ++
     " br i1 %112,label %l24,label %l26" ++
     " l24:" ++
     " %113 = icmp ne i2 2,%111" ++
     " br i1 %113,label %l25,label %l27" ++
     " l25:" ++
     " %114 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 2" ++
     " %115 = load i8*,i8** %114" ++
     " %116 = bitcast i8* %115 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l27" ++
     " l26:" ++
     " %117 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 2" ++
     " %118 = load i8*,i8** %117" ++
     " %119 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 3" ++
     " %120 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107 to i8*" ++
     " %121 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %119" ++
     " %122 = call fastcc {i2,i8*} %121(i8* %118,i8* %120)" ++
     " %123 = extractvalue {i2,i8*} %122,0" ++
     " %124 = extractvalue {i2,i8*} %122,1" ++
     " %125 = bitcast i8* %124 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " br label %l27" ++
     " l27:" ++
     " %126 = phi i2 [%111,%l24],[%111,%l25],[%123,%l26]" ++
     " %127 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l24],[%116,%l25],[%125,%l26]" ++
     " %128 = icmp eq i2 3,%126" ++
     " br i1 %128,label %l28,label %l29" ++
     " l28:" ++
     " call void @abort() noreturn" ++
     " ret void" ++
     " l29:" ++
     " %129 = icmp eq i2 2,%126" ++
     " br i1 %129,label %l30,label %l31" ++
     " l30:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107)" ++
     " call void @exit(i32 0) noreturn" ++
     " ret void" ++
     " l31:" ++
     " %130 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %127,i32 0,i32 0" ++
     " %131 = load i32,i32* %130" ++
     " %132 = add i32 1,%131" ++
     " store i32 %132,i32* %130" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107)" ++
     " %133 = zext i2 %126 to i8" ++
     " %134 = shl i8 %133,%108" ++
     " %135 = or i8 %109,%134" ++
     " %136 = sub i8 %108,1" ++
     " %137 = icmp sge i8 %136,0" ++
     " br i1 %137,label %l23,label %l32" ++
     " l32:" ++
     " store i8 %135,i8* %106" ++
     " %138 = call i32 @write(i32 1,i8* %106,i32 1)" ++
     " br label %l23" ++
     " }")
