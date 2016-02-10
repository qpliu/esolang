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
     -- %12 = cdr first, when first is already evaluated
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
     -- %21 = cdr first, after forcing evaluation of first
     " br label %l4" ++
     " l4:" ++
     " %22 = phi i2 [%7,%l1],[%7,%l2],[%19,%l3]" ++
     " %23 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l1],[%12,%l2],[%21,%l3]" ++
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
     " %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 1" ++
     " %43 = load i2,i2* %42" ++
     " %44 = icmp ne i2 3,%43" ++
     " br i1 %44,label %l9,label %l11" ++
     " l9: %45 = icmp ne i2 2,%43" ++
     " br i1 %45,label %l10,label %l12" ++
     " l10:" ++
     " %46 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2" ++
     " %47 = load i8*,i8** %46" ++
     " %48 = bitcast i8* %47 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     -- %48 = cdr second, when already evaluated
     " br label %l12" ++
     " l11:" ++
     " %49 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 2" ++
     " %50 = load i8*,i8** %49" ++
     " %51 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5,i32 0,i32 3" ++
     " %52 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5 to i8*" ++
     " %53 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %51" ++
     " %54 = call fastcc {i2,i8*} %53(i8* %50,i8* %52)" ++
     " %55 = extractvalue {i2,i8*} %54,0" ++
     " %56 = extractvalue {i2,i8*} %54,1" ++
     " %57 = bitcast i8* %56 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     -- %57 = cdr second, after forcing evaluation of second
     " br label %l12" ++
     " l12:" ++
     " %58 = phi i2 [%43,%l9],[%43,%l10],[%55,%l11]" ++
     " %59 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l9],[%48,%l10],[%57,%l11]" ++
     " %60 = icmp eq i2 3,%58" ++
     " br i1 %60,label %l5,label %l13" ++
     " l13:" ++
     " %61 = icmp ne i2 2,%58" ++
     " br i1 %61,label %l14,label %l15" ++
     " l14:" ++
     " %62 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %59,i32 0,i32 0" ++
     " %63 = load i32,i32* %62" ++
     " %64 = add i32 1,%63" ++
     " store i32 %64,i32* %62" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5)" ++
     " call void @free(i8* %evalParam)" ++
     " %65 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %59 to i8*" ++
     " %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 %58,i2* %66" ++
     " %67 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %65,i8** %67" ++
     " %68 = insertvalue {i2,i8*} undef,i2 %58,0" ++
     " %69 = insertvalue {i2,i8*} %68,i8* %65,1" ++
     " ret {i2,i8*} %69" ++
     " l15:" ++
     " call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %5)" ++
     " call void @free(i8* %evalParam)" ++
     " %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 2,i2* %70" ++
     " %71 = insertvalue {i2,i8*} undef,i2 %58,0" ++
     " ret {i2,i8*} %71" ++
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
     " %6 = icmp uge i8 %5,8" ++
     " br i1 %6,label %l1,label %l2" ++
     " l1:" ++
     " %7 = getelementptr {i32,i8,i8},{i32,i8,i8}* %0,i32 0,i32 0" ++
     " %8 = load i32,i32* %7" ++
     " %9 = call i32 @read(i32 %8,i8* %4,i32 1)" ++
     " %10 = icmp eq i32 1,%9" ++
     " br i1 %10,label %l2,label %l3" ++
     " l2:" ++
     " %11 = phi i8 [%5,%l0],[0,%l1]" ++
     " %12 = add i8 1,%11" ++
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
     " }")

testSimple :: Assertion
testSimple = testCodeGen "testSimple" "f=."
    ("@L = private constant [0 x i1] []" ++
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
     " %6 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* null,i32 0,i32 1,i32 0" ++
     " %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}** %6 to i32" ++
     " %8 = call i8* @malloc(i32 %7)" ++
     " %9 = bitcast i8* %8 to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}*" ++
     " %10 = getelementptr {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]},{i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}* %9,i32 0,i32 0" ++
     " store i32 0,i32* %10" ++
     " %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 2" ++
     " store i8* %8,i8** %11" ++
     " %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalFunc_f,{i2,i8*}(i8*,i8*)** %12" ++
     " %13 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamFunc,void(i8*)** %13" ++
     " ret {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3" ++
     " }" ++
     "define private fastcc {i2,i8*} @evalFunc_f(i8* %evalParam,i8* %value) {" ++
     " l0:" ++
     " %0 = bitcast i8* %evalParam to {i32,[0 x {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*]}*" ++
     " %1 = bitcast i8* %value to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " call void @free(i8* %evalParam)" ++
     " br label %l1" ++
     " l1:" ++
     " %2 = bitcast [0 x i1]* @L to [0 x i1]*" ++
     " %3 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1" ++
     " %4 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3 to i32" ++
     " %5 = call i8* @malloc(i32 %4)" ++
     " %6 = bitcast i8* %5 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %7 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 0" ++
     " store i32 1,i32* %7" ++
     " %8 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 1" ++
     " store i2 3,i2* %8" ++
     " %9 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* null,i32 1" ++
     " %10 = ptrtoint {[0 x i1]*,i32,i32}* %9 to i32" ++
     " %11 = call i8* @malloc(i32 %10)" ++
     " %12 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2" ++
     " store i8* %11,i8** %12" ++
     " %13 = bitcast i8* %11 to {[0 x i1]*,i32,i32}*" ++
     " %14 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 0" ++
     " store [0 x i1]* %2,[0 x i1]** %14" ++
     " %15 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 1" ++
     " store i32 0,i32* %15" ++
     " %16 = getelementptr {[0 x i1]*,i32,i32},{[0 x i1]*,i32,i32}* %13,i32 0,i32 2" ++
     " store i32 0,i32* %16" ++
     " %17 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3" ++
     " store {i2,i8*}(i8*,i8*)* @evalLiteral,{i2,i8*}(i8*,i8*)** %17" ++
     " %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 4" ++
     " store void(i8*)* @freeEvalParamLiteral,void(i8*)** %18" ++
     " %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 1" ++
     " %20 = load i2,i2* %19" ++
     " %21 = icmp ne i2 3,%20" ++
     " br i1 %21,label %l2,label %l5" ++
     " l2:" ++
     " %22 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 %20,i2* %22" ++
     " %23 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2" ++
     " %24 = load i8*,i8** %23" ++
     " %25 = icmp ne i2 2,%20" ++
     " br i1 %25,label %l3,label %l4" ++
     " l3:" ++
     " %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %24,i8** %26" ++
     " %27 = bitcast i8* %24 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %28 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %27,i32 0,i32 0" ++
     " %29 = load i32,i32* %28" ++
     " %30 = add i32 1,%29" ++
     " store i32 %30,i32* %28" ++
     " br label %l4" ++
     " l4:" ++
     " %31 = insertvalue {i2,i8*} undef,i2 %20,0" ++
     " %32 = insertvalue {i2,i8*} %31,i8* %24,1" ++
     " ret {i2,i8*} %32" ++
     " l5:" ++
     " %33 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 0" ++
     " %34 = load i32,i32* %33" ++
     " %35 = icmp ugt i32 %34,1" ++
     " br i1 %35,label %l6,label %l7" ++
     " l6:" ++
     " %36 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2" ++
     " %37 = load i8*,i8** %36" ++
     " %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3" ++
     " %39 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i8*" ++
     " %40 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %38" ++
     " %41 = call fastcc {i2,i8*} %40(i8* %37,i8* %39)" ++
     " %42 = extractvalue {i2,i8*} %41,0" ++
     " %43 = extractvalue {i2,i8*} %41,1" ++
     " %44 = bitcast i8* %43 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}*" ++
     " %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 1" ++
     " store i2 %42,i2* %45" ++
     " %46 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %1,i32 0,i32 2" ++
     " store i8* %43,i8** %46" ++
     " %47 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %44,i32 0,i32 0" ++
     " %48 = load i32,i32* %47" ++
     " %49 = add i32 1,%48" ++
     " store i32 %49,i32* %47" ++
     " ret {i2,i8*} %41" ++
     " l7:" ++
     " %50 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 2" ++
     " %51 = load i8*,i8** %50" ++
     " %52 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6,i32 0,i32 3" ++
     " %53 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %52" ++
     " %54 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i8*" ++
     " call void @free(i8* %54)" ++
     " %55 = musttail call fastcc {i2,i8*} %53(i8* %51,i8* %value)" ++
     " ret {i2,i8*} %55" ++
     " }")

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
     " %3 = phi i8 [0,%l1],[%31,%l10],[0,%l11]" ++
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
     " %31 = add i8 1,%3" ++
     " %32 = icmp ult i8 %31,8" ++
     " br i1 %32,label %l2,label %l11" ++
     " l11:" ++
     " store i8 %30,i8* %1" ++
     " %33 = call i32 @write(i32 1,i8* %1,i32 1)" ++
     " br label %l2" ++
     " }")

testGenMain1 :: Assertion
testGenMain1 = testGenMain "testGenMain1" "f a=."
    ("define void @main(i32 %argc,i8** %argv) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 2,i2* %5 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1 store i2 3,i2* %11 %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %13 = ptrtoint {i32,i8,i8}* %12 to i32 %14 = call i8* @malloc(i32 %13) %15 = bitcast i8* %14 to {i32,i8,i8}* %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0 store i32 0,i32* %16 %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1 store i8 8,i8* %17 %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2 store i8* %14,i8** %18 %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19 %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %20 %21 = icmp ult i32 1,%argc br i1 %21,label %l1,label %l4 l1: %22 = getelementptr i8*,i8** %argv,i32 1 %23 = load i8*,i8** %22 %24 = call i32 @open(i8* %23,i32 0) %25 = icmp sge i32 %24,0 br i1 %25,label %l2,label %l3 l2: %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32 %28 = call i8* @malloc(i32 %27) %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0 store i32 1,i32* %30 %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1 store i2 3,i2* %31 %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %33 = ptrtoint {i32,i8,i8}* %32 to i32 %34 = call i8* @malloc(i32 %33) %35 = bitcast i8* %34 to {i32,i8,i8}* %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0 store i32 %24,i32* %36 %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1 store i8 8,i8* %37 %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2 store i8* %34,i8** %38 %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39 %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %40 br label %l7 l3: call void @perror(i8* %23) call void @exit(i32 -1) noreturn ret void l4: %41 = icmp eq i32 1,%argc br i1 %41,label %l5,label %l6 l5: %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %43 = load i32,i32* %42 %44 = add i32 1,%43 store i32 %44,i32* %42 br label %l7 l6: %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %46 = load i32,i32* %45 %47 = add i32 1,%46 store i32 %47,i32* %45 br label %l7 l7: %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6] call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9) %49 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48) br label %l8 l8: %50 = alloca i8,i32 1 br label %l9 l9: %51 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%49,%l8],[%71,%l18],[%71,%l17] %52 = phi i8 [0,%l8],[%80,%l17],[0,%l18] %53 = phi i8 [0,%l8],[%79,%l17],[0,%l18] %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 1 %55 = load i2,i2* %54 %56 = icmp ne i2 3,%55 br i1 %56,label %l10,label %l12 l10: %57 = icmp ne i2 2,%55 br i1 %57,label %l11,label %l13 l11: %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 2 %59 = load i8*,i8** %58 %60 = bitcast i8* %59 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l13 l12: %61 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 2 %62 = load i8*,i8** %61 %63 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51,i32 0,i32 3 %64 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51 to i8* %65 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %63 %66 = call fastcc {i2,i8*} %65(i8* %62,i8* %64) %67 = extractvalue {i2,i8*} %66,0 %68 = extractvalue {i2,i8*} %66,1 %69 = bitcast i8* %68 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l13 l13: %70 = phi i2 [%55,%l10],[%55,%l11],[%67,%l12] %71 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l10],[%60,%l11],[%69,%l12] %72 = icmp eq i2 3,%70 br i1 %72,label %l14,label %l15 l14: call void @abort() noreturn ret void l15: %73 = icmp eq i2 2,%70 br i1 %73,label %l16,label %l17 l16: call void @exit(i32 0) noreturn ret void l17: %74 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %71,i32 0,i32 0 %75 = load i32,i32* %74 %76 = add i32 1,%75 store i32 %76,i32* %74 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %51) %77 = zext i2 %70 to i8 %78 = shl i8 %77,%52 %79 = or i8 %53,%78 %80 = add i8 1,%52 %81 = icmp ult i8 %80,8 br i1 %81,label %l9,label %l18 l18: store i8 %79,i8* %50 %82 = call i32 @write(i32 1,i8* %50,i32 1) br label %l9 }")

testGenMain2 :: Assertion
testGenMain2 = testGenMain "testGenMain2" "f a b=."
    ("define void @main(i32 %argc,i8** %argv) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 2,i2* %5 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1 store i2 3,i2* %11 %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %13 = ptrtoint {i32,i8,i8}* %12 to i32 %14 = call i8* @malloc(i32 %13) %15 = bitcast i8* %14 to {i32,i8,i8}* %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0 store i32 0,i32* %16 %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1 store i8 8,i8* %17 %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2 store i8* %14,i8** %18 %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19 %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %20 %21 = icmp ult i32 1,%argc br i1 %21,label %l1,label %l4 l1: %22 = getelementptr i8*,i8** %argv,i32 1 %23 = load i8*,i8** %22 %24 = call i32 @open(i8* %23,i32 0) %25 = icmp sge i32 %24,0 br i1 %25,label %l2,label %l3 l2: %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32 %28 = call i8* @malloc(i32 %27) %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0 store i32 1,i32* %30 %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1 store i2 3,i2* %31 %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %33 = ptrtoint {i32,i8,i8}* %32 to i32 %34 = call i8* @malloc(i32 %33) %35 = bitcast i8* %34 to {i32,i8,i8}* %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0 store i32 %24,i32* %36 %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1 store i8 8,i8* %37 %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2 store i8* %34,i8** %38 %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39 %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %40 br label %l7 l3: call void @perror(i8* %23) call void @exit(i32 -1) noreturn ret void l4: %41 = icmp eq i32 1,%argc br i1 %41,label %l5,label %l6 l5: %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %43 = load i32,i32* %42 %44 = add i32 1,%43 store i32 %44,i32* %42 br label %l7 l6: %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %46 = load i32,i32* %45 %47 = add i32 1,%46 store i32 %47,i32* %45 br label %l7 l7: %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6] %49 = icmp ult i32 2,%argc br i1 %49,label %l8,label %l11 l8: %50 = getelementptr i8*,i8** %argv,i32 2 %51 = load i8*,i8** %50 %52 = call i32 @open(i8* %51,i32 0) %53 = icmp sge i32 %52,0 br i1 %53,label %l9,label %l10 l9: %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %55 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %54 to i32 %56 = call i8* @malloc(i32 %55) %57 = bitcast i8* %56 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 0 store i32 1,i32* %58 %59 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 1 store i2 3,i2* %59 %60 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %61 = ptrtoint {i32,i8,i8}* %60 to i32 %62 = call i8* @malloc(i32 %61) %63 = bitcast i8* %62 to {i32,i8,i8}* %64 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 0 store i32 %52,i32* %64 %65 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 1 store i8 8,i8* %65 %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 2 store i8* %62,i8** %66 %67 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %67 %68 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %68 br label %l14 l10: call void @perror(i8* %51) call void @exit(i32 -1) noreturn ret void l11: %69 = icmp eq i32 2,%argc br i1 %69,label %l12,label %l13 l12: %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %71 = load i32,i32* %70 %72 = add i32 1,%71 store i32 %72,i32* %70 br label %l14 l13: %73 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %74 = load i32,i32* %73 %75 = add i32 1,%74 store i32 %75,i32* %73 br label %l14 l14: %76 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%57,%l9],[%9,%l12],[%3,%l13] call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9) %77 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %76) br label %l15 l15: %78 = alloca i8,i32 1 br label %l16 l16: %79 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%77,%l15],[%99,%l25],[%99,%l24] %80 = phi i8 [0,%l15],[%108,%l24],[0,%l25] %81 = phi i8 [0,%l15],[%107,%l24],[0,%l25] %82 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 1 %83 = load i2,i2* %82 %84 = icmp ne i2 3,%83 br i1 %84,label %l17,label %l19 l17: %85 = icmp ne i2 2,%83 br i1 %85,label %l18,label %l20 l18: %86 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 2 %87 = load i8*,i8** %86 %88 = bitcast i8* %87 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l20 l19: %89 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 2 %90 = load i8*,i8** %89 %91 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79,i32 0,i32 3 %92 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79 to i8* %93 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %91 %94 = call fastcc {i2,i8*} %93(i8* %90,i8* %92) %95 = extractvalue {i2,i8*} %94,0 %96 = extractvalue {i2,i8*} %94,1 %97 = bitcast i8* %96 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l20 l20: %98 = phi i2 [%83,%l17],[%83,%l18],[%95,%l19] %99 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l17],[%88,%l18],[%97,%l19] %100 = icmp eq i2 3,%98 br i1 %100,label %l21,label %l22 l21: call void @abort() noreturn ret void l22: %101 = icmp eq i2 2,%98 br i1 %101,label %l23,label %l24 l23: call void @exit(i32 0) noreturn ret void l24: %102 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %99,i32 0,i32 0 %103 = load i32,i32* %102 %104 = add i32 1,%103 store i32 %104,i32* %102 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %79) %105 = zext i2 %98 to i8 %106 = shl i8 %105,%80 %107 = or i8 %81,%106 %108 = add i8 1,%80 %109 = icmp ult i8 %108,8 br i1 %109,label %l16,label %l25 l25: store i8 %107,i8* %78 %110 = call i32 @write(i32 1,i8* %78,i32 1) br label %l16 }")

testGenMain3 :: Assertion
testGenMain3 = testGenMain "testGenMain3" "f a b c=."
    ("define void @main(i32 %argc,i8** %argv) { l0: %0 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %1 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %0 to i32 %2 = call i8* @malloc(i32 %1) %3 = bitcast i8* %2 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %4 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 store i32 1,i32* %4 %5 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 1 store i2 2,i2* %5 %6 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %7 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %6 to i32 %8 = call i8* @malloc(i32 %7) %9 = bitcast i8* %8 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %10 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 store i32 1,i32* %10 %11 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 1 store i2 3,i2* %11 %12 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %13 = ptrtoint {i32,i8,i8}* %12 to i32 %14 = call i8* @malloc(i32 %13) %15 = bitcast i8* %14 to {i32,i8,i8}* %16 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 0 store i32 0,i32* %16 %17 = getelementptr {i32,i8,i8},{i32,i8,i8}* %15,i32 0,i32 1 store i8 8,i8* %17 %18 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 2 store i8* %14,i8** %18 %19 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %19 %20 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %20 %21 = icmp ult i32 1,%argc br i1 %21,label %l1,label %l4 l1: %22 = getelementptr i8*,i8** %argv,i32 1 %23 = load i8*,i8** %22 %24 = call i32 @open(i8* %23,i32 0) %25 = icmp sge i32 %24,0 br i1 %25,label %l2,label %l3 l2: %26 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %27 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %26 to i32 %28 = call i8* @malloc(i32 %27) %29 = bitcast i8* %28 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %30 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 0 store i32 1,i32* %30 %31 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 1 store i2 3,i2* %31 %32 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %33 = ptrtoint {i32,i8,i8}* %32 to i32 %34 = call i8* @malloc(i32 %33) %35 = bitcast i8* %34 to {i32,i8,i8}* %36 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 0 store i32 %24,i32* %36 %37 = getelementptr {i32,i8,i8},{i32,i8,i8}* %35,i32 0,i32 1 store i8 8,i8* %37 %38 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 2 store i8* %34,i8** %38 %39 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %39 %40 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %29,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %40 br label %l7 l3: call void @perror(i8* %23) call void @exit(i32 -1) noreturn ret void l4: %41 = icmp eq i32 1,%argc br i1 %41,label %l5,label %l6 l5: %42 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %43 = load i32,i32* %42 %44 = add i32 1,%43 store i32 %44,i32* %42 br label %l7 l6: %45 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %46 = load i32,i32* %45 %47 = add i32 1,%46 store i32 %47,i32* %45 br label %l7 l7: %48 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%29,%l2],[%9,%l5],[%3,%l6] %49 = icmp ult i32 2,%argc br i1 %49,label %l8,label %l11 l8: %50 = getelementptr i8*,i8** %argv,i32 2 %51 = load i8*,i8** %50 %52 = call i32 @open(i8* %51,i32 0) %53 = icmp sge i32 %52,0 br i1 %53,label %l9,label %l10 l9: %54 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %55 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %54 to i32 %56 = call i8* @malloc(i32 %55) %57 = bitcast i8* %56 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %58 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 0 store i32 1,i32* %58 %59 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 1 store i2 3,i2* %59 %60 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %61 = ptrtoint {i32,i8,i8}* %60 to i32 %62 = call i8* @malloc(i32 %61) %63 = bitcast i8* %62 to {i32,i8,i8}* %64 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 0 store i32 %52,i32* %64 %65 = getelementptr {i32,i8,i8},{i32,i8,i8}* %63,i32 0,i32 1 store i8 8,i8* %65 %66 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 2 store i8* %62,i8** %66 %67 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %67 %68 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %57,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %68 br label %l14 l10: call void @perror(i8* %51) call void @exit(i32 -1) noreturn ret void l11: %69 = icmp eq i32 2,%argc br i1 %69,label %l12,label %l13 l12: %70 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %71 = load i32,i32* %70 %72 = add i32 1,%71 store i32 %72,i32* %70 br label %l14 l13: %73 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %74 = load i32,i32* %73 %75 = add i32 1,%74 store i32 %75,i32* %73 br label %l14 l14: %76 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%57,%l9],[%9,%l12],[%3,%l13] %77 = icmp ult i32 3,%argc br i1 %77,label %l15,label %l18 l15: %78 = getelementptr i8*,i8** %argv,i32 3 %79 = load i8*,i8** %78 %80 = call i32 @open(i8* %79,i32 0) %81 = icmp sge i32 %80,0 br i1 %81,label %l16,label %l17 l16: %82 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* null,i32 1 %83 = ptrtoint {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %82 to i32 %84 = call i8* @malloc(i32 %83) %85 = bitcast i8* %84 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %86 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 0 store i32 1,i32* %86 %87 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 1 store i2 3,i2* %87 %88 = getelementptr {i32,i8,i8},{i32,i8,i8}* null,i32 1 %89 = ptrtoint {i32,i8,i8}* %88 to i32 %90 = call i8* @malloc(i32 %89) %91 = bitcast i8* %90 to {i32,i8,i8}* %92 = getelementptr {i32,i8,i8},{i32,i8,i8}* %91,i32 0,i32 0 store i32 %80,i32* %92 %93 = getelementptr {i32,i8,i8},{i32,i8,i8}* %91,i32 0,i32 1 store i8 8,i8* %93 %94 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 2 store i8* %90,i8** %94 %95 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 3 store {i2,i8*}(i8*,i8*)* @evalFile,{i2,i8*}(i8*,i8*)** %95 %96 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %85,i32 0,i32 4 store void(i8*)* @freeEvalParamFile,void(i8*)** %96 br label %l21 l17: call void @perror(i8* %79) call void @exit(i32 -1) noreturn ret void l18: %97 = icmp eq i32 3,%argc br i1 %97,label %l19,label %l20 l19: %98 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9,i32 0,i32 0 %99 = load i32,i32* %98 %100 = add i32 1,%99 store i32 %100,i32* %98 br label %l21 l20: %101 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3,i32 0,i32 0 %102 = load i32,i32* %101 %103 = add i32 1,%102 store i32 %103,i32* %101 br label %l21 l21: %104 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%85,%l16],[%9,%l19],[%3,%l20] call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %3) call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %9) %105 = call fastcc {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* @_f({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %48,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %76,{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %104) br label %l22 l22: %106 = alloca i8,i32 1 br label %l23 l23: %107 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [%105,%l22],[%127,%l32],[%127,%l31] %108 = phi i8 [0,%l22],[%136,%l31],[0,%l32] %109 = phi i8 [0,%l22],[%135,%l31],[0,%l32] %110 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 1 %111 = load i2,i2* %110 %112 = icmp ne i2 3,%111 br i1 %112,label %l24,label %l26 l24: %113 = icmp ne i2 2,%111 br i1 %113,label %l25,label %l27 l25: %114 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 2 %115 = load i8*,i8** %114 %116 = bitcast i8* %115 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l27 l26: %117 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 2 %118 = load i8*,i8** %117 %119 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107,i32 0,i32 3 %120 = bitcast {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107 to i8* %121 = load {i2,i8*}(i8*,i8*)*,{i2,i8*}(i8*,i8*)** %119 %122 = call fastcc {i2,i8*} %121(i8* %118,i8* %120) %123 = extractvalue {i2,i8*} %122,0 %124 = extractvalue {i2,i8*} %122,1 %125 = bitcast i8* %124 to {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* br label %l27 l27: %126 = phi i2 [%111,%l24],[%111,%l25],[%123,%l26] %127 = phi {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* [null,%l24],[%116,%l25],[%125,%l26] %128 = icmp eq i2 3,%126 br i1 %128,label %l28,label %l29 l28: call void @abort() noreturn ret void l29: %129 = icmp eq i2 2,%126 br i1 %129,label %l30,label %l31 l30: call void @exit(i32 0) noreturn ret void l31: %130 = getelementptr {i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*},{i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %127,i32 0,i32 0 %131 = load i32,i32* %130 %132 = add i32 1,%131 store i32 %132,i32* %130 call fastcc void @unref({i32,i2,i8*,{i2,i8*}(i8*,i8*)*,void(i8*)*}* %107) %133 = zext i2 %126 to i8 %134 = shl i8 %133,%108 %135 = or i8 %109,%134 %136 = add i8 1,%108 %137 = icmp ult i8 %136,8 br i1 %137,label %l23,label %l32 l32: store i8 %135,i8* %106 %138 = call i32 @write(i32 1,i8* %106,i32 1) br label %l23 }")
