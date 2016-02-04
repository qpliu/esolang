module TestCodeGen
    (tests)
where

import Control.Monad(unless)
import System.Exit(ExitCode(ExitSuccess))
import System.Process(readProcessWithExitCode)
import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)

import Compile(CompileError,compile)
import Parse(parse)
import Resolve(resolve)
import CodeGen(codeGen,genMain)

tests :: Test
tests = TestList [
    TestCase testSimple
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
    ("define private fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 0 %1 = load i32,i32* %0 %2 = sub i32 %1,1 %3 = icmp ugt i32 %2,0 br i1 %3,label %l1,label %l2 l1: store i32 %2,i32* %0 ret void l2: %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 1 %5 = load i2,i2* %4 %6 = icmp eq i2 3,%5 br i1 %6,label %l3,label %l4 l3: %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 2 %8 = load i8*,i8** %7 %9 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 4 %10 = load void(i8*)*,void(i8*)** %9 call fastcc void %10(i8* %8) %11 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8* call void @free(i8* %11) ret void l4: %12 = icmp eq i2 2,%5 br i1 %12,label %l5,label %l6 l5: %13 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8* call void @free(i8* %13) ret void l6: %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value,i32 0,i32 2 %15 = load i8*,i8** %14 %16 = bitcast i8* %15 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %value to i8* call void @free(i8* %17) musttail call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %16) ret void }" ++
     "define private fastcc {i2,i8*} @evalLiteral(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %evalParam to {[0 x i1]*,i32,i32}* %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 %3 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 1 %4 = load i32,i32* %3 %5 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 2 %6 = load i32,i32* %5 %7 = icmp ult i32 %4,%6 br i1 %7,label %l1,label %l2 l1: %8 = add i32 1,%4 store i32 %8,i32* %3 %9 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %0,i32 0,i32 0 %10 = load [0 x i1]*,[0 x i1]** %9 %11 = getelementptr [0 x i1],[0 x i1]*%10,i32 0,i32 %4 %12 = load i1,i1* %11 %13 = zext i1 %12 to i2 store i2 %13,i2* %2 %14 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %15 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %14 to i32 %16 = call i8* @malloc(i32 %15) %17 = bitcast i8* %16 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 0 store i32 1,i32* %18 %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 1 store i2 3,i2 *%19 %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %16,i8** %20 %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 2 store i8* %evalParam,i8** %21 %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalLiteral,{i2,i8*}(i8*,i8*)** %22 %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17,i32 0,i32 4 store void(i8*)* @freeEvalParamLiteral,void(i8*)** %23 %24 = insertvalue {i2,i8*} undef,i2 %13,0 %25 = insertvalue {i2,i8*} %24,i8* %16,1 ret {i2,i8*} %25 l2: call void @free(i8* %evalParam) store i2 2,i2* %2 %26 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %26 }" ++
     "define private fastcc void @freeEvalParamLiteral(i8* %evalParam) { call void @free(i8* %evalParam) ret void }" ++
     "define private fastcc {i2,i8*} @evalConcat(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %evalParam to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 0 %3 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %2 %4 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 1 %5 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %4 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 %7 = load i8*,i8** %6 %8 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 %9 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 to i8* %10 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %8 %11 = call fastcc {i2,i8*} %10(i8* %7,i8* %9) %12 = extractvalue {i2,i8*} %11,0 %13 = extractvalue {i2,i8*} %11,1 %14 = bitcast i8* %13 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %15 = icmp eq i2 3,%12 br i1 %15,label %l1,label %l2 l1: call void @abort() noreturn ret {i2,i8*} undef l2: %16 = icmp ne i2 2,%12 br i1 %16,label %l3,label %l4 l3: %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %14,i32 0,i32 0 %18 = load i32,i32* %17 %19 = add i32 1,%18 store i32 %19,i32* %17 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %21 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20 to i32 %22 = call i8* @malloc(i32 %21) %23 = bitcast i8* %22 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 0 store i32 1,i32* %24 %25 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 1 store i2 3,i2 *%25 %26 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* null,i32 1 %27 = ptrtoint {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %26 to i32 %28 = call i8* @malloc(i32 %27) %29 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 2 store i8* %28,i8** %29 %30 = bitcast i8* %28 to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %31 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %30,i32 0,i32 0 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %14,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %31 %32 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %30,i32 0,i32 1 store {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %32 %33 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalConcat,{i2,i8*}(i8*,i8*)** %33 %34 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %23,i32 0,i32 4 store void(i8*)* @freeEvalParamConcat,void(i8*)** %34 %35 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 store i2 %12,i2* %35 %36 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %22,i8** %36 %37 = insertvalue {i2,i8*} undef,i2 %12,0 %38 = insertvalue {i2,i8*} %37,i8* %22,1 ret {i2,i8*} %38 l4: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2 %40 = load i8*,i8** %39 %41 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 3 %42 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5 to i8* %43 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %41 %44 = call fastcc {i2,i8*} %43(i8* %40,i8* %42) %45 = extractvalue {i2,i8*} %44,0 %46 = extractvalue {i2,i8*} %44,1 %47 = bitcast i8* %46 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48 = icmp eq i2 3,%45 br i1 %48,label %l1,label %l5 l5: %49 = icmp ne i2 2,%45 br i1 %49,label %l6,label %l7 l6: %50 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %47,i32 0,i32 0 %51 = load i32,i32* %50 %52 = add i32 1,%51 store i32 %52,i32* %50 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5) %53 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %47 to i8* %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 store i2 %45,i2* %54 %55 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %53,i8** %55 %56 = insertvalue {i2,i8*} undef,i2 %45,0 %57 = insertvalue {i2,i8*} %56,i8* %53,1 ret {i2,i8*} %57 l7: call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5) call void @free(i8* %evalParam) %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 store i2 2,i2* %58 %59 = insertvalue {i2,i8*} undef,i2 %45,0 ret {i2,i8*} %59 }" ++
     "define private fastcc void @freeEvalParamConcat(i8* %evalParam) { l0: %0 = bitcast i8* %evalParam to {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %1 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 0 %2 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %1 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2) %3 = getelementptr {{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*},{{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*}* %0,i32 0,i32 1 %4 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %3 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4) call void @free(i8* %evalParam) ret void }" ++
     "define private fastcc {i2,i8*} @evalFile(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %evalParam to {i32,i8,i8}* %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %2 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 %3 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 1 %4 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 2 %5 = load i8,i8* %3 %6 = icmp uge i8 %5,8 br i1 %6,label %l1,label %l2 l1: %7 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 0 %8 = load i32,i32* %7 %9 = call i32 @read(i32 %8,i8* %4,i32 1) %10 = icmp eq i32 1,%9 br i1 %10,label %l2,label %l3 l2: %11 = phi i8 [%5,%l0],[0,%l1] %12 = add i8 1,%11 store i8 %12,i8* %3 %13 = load i8,i8* %4 %14 = lshr i8 %13,%11 %15 = trunc i8 %14 to i2 %16 = and i2 1,%15 store i2 %16,i2* %2 %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %18 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %17 to i32 %19 = call i8* @malloc(i32 %18) %20 = bitcast i8* %19 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %21 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 0 store i32 1,i32* %21 %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 1 store i2 3,i2 *%22 %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %19,i8** %23 %24 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %24 %25 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %20,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %25 %26 = insertvalue {i2,i8*} undef,i2 %16,0 %27 = insertvalue {i2,i8*} %26,i8* %19,1 ret {i2,i8*} %27 l3: %28 =  call i32 @close(i32 %8) call void @free(i8* %evalParam) store i2 2,i2* %2 %29 = insertvalue {i2,i8*} undef,i2 2,0 ret {i2,i8*} %29 }" ++
     "define private fastcc void @freeEvalParamFile(i8* %evalParam) { l0: %0 = bitcast i8* %evalParam to {i32,i8,i8}* %1 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 0 %2 = load i32,i32* %1 %3 =  call i32 @close(i32 %2) call void @free(i8* %evalParam) ret void }" ++
     "define private fastcc void @freeEvalParamFunc(i8* %evalParam) { l0: %0 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %0,i32 0,i32 0 %2 = load i32,i32* %1 br label %l1 l1: %3 = phi i32 [0,%l0],[%7,%l2] %4 = icmp ult i32 %3,%2 br i1 %4,label %l2,label %l3 l2: %5 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %0,i32 0,i32 1,i32 %3 %6 = load {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %5 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6) %7 = add i32 1,%3 br label %l1 l3: call void @free(i8* %evalParam) ret void }")

testSimple :: Assertion
testSimple = testCodeGen "testSimple" "f=."
    ("@L = private constant [0 x i1] []" ++
     declares ++ defns ++
     "define fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f() { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 3,i2 *%5 %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 0 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0 store i32 0,i32* %10 %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2 store i8* %8,i8** %11 %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %12 %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4 store void(i8*)* @freeEvalParamFunc,void(i8*)** %13 ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 }define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) { l0: %0 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* call void @free(i8* %evalParam) br label %l1 l1: %2 = bitcast [0 x i1]* @L to [0 x i1]* %3 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %4 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 to i32 %5 = call i8* @malloc(i32 %4) %6 = bitcast i8* %5 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 0 store i32 1,i32* %7 %8 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 1 store i2 3,i2 *%8 %9 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* null,i32 1 %10 = ptrtoint {[0 x i1]*,i32,i32}* %9 to i32 %11 = call i8* @malloc(i32 %10) %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2 store i8* %11,i8** %12 %13 = bitcast i8* %11 to {[0 x i1]*,i32,i32}* %14 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 0 store [0 x i1]* %2,[0 x i1]** %14 %15 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 1 store i32 0,i32* %15 %16 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 1 store i32 0,i32* %16 %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalLiteral,{i2,i8*}(i8*,i8*)** %17 %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 4 store void(i8*)* @freeEvalParamLiteral,void(i8*)** %18 %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 1 %20 = load i2,i2* %19 %21 = icmp ne i2 3,%20 br i1 %21,label %l2,label %l5 l2: %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 store i2 %20,i2* %22 %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2 %24 = load i8*,i8** %23 %25 = icmp eq i2 2,%20 br i1 %25,label %l3,label %l4 l3: %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %24,i8** %26 %27 = bitcast i8* %24 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %28 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %27,i32 0,i32 0 %29 = load i32,i32* %28 %30 = add i32 1,%29 store i32 %30,i32* %28 br label %l4 l4: %31 = insertvalue {i2,i8*} undef,i2 %20,0 %32 = insertvalue {i2,i8*} %31,i8* %24,1 ret {i2,i8*} %32 l5: %33 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 0 %34 = load i32,i32* %33 %35 = icmp ugt i32 %34,1 br i1 %35,label %l6,label %l7 l6: %36 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2 %37 = load i8*,i8** %36 %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3 %39 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i8* %40 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %38 %41 = call fastcc {i2,i8*} %40(i8* %37,i8* %39) %42 = extractvalue {i2,i8*} %41,0 %43 = extractvalue {i2,i8*} %41,1 %44 = bitcast i8* %43 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1 store i2 %42,i2* %45 %46 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2 store i8* %43,i8** %46 %47 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %44,i32 0,i32 0 %48 = load i32,i32* %47 %49 = add i32 1,%48 store i32 %49,i32* %47 ret {i2,i8*} %41 l7: %50 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2 %51 = load i8*,i8** %50 %52 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3 %53 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %52 %54 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i8* call void @free(i8* %54) %55 = musttail call fastcc {i2,i8*} %53(i8* %51,i8* %value) ret {i2,i8*} %55 }")