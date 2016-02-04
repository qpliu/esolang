module TestResolve
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertFailure)

import Ast(Identifier(..),Param(..),Def(..),Expr(..),Func)
import Compile(compile)
import Parse(parse)
import Resolve(resolve)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testLiteral,
    TestCase testBound,
    TestCase testFuncall,
    TestCase testConcat
    ]

testResolve :: String -> String -> ([Func] -> Bool) -> Assertion
testResolve label code checkResult =
    either (assertFailure . ((label ++ ":") ++) . show)
           (\ result ->
                if checkResult result then return () else assertFailure label)
           (compile (parse "(test)" code >>= resolve))

testSimple :: Assertion
testSimple = testResolve "testSimple" "f=." checkResult
  where
    checkResult [("f",[Def [] (ExprLiteral _ [])])] = True
    checkResult _ = False

testLiteral :: Assertion
testLiteral = testResolve "testLiteral" "f=0110." checkResult
  where
    checkResult [("f",[Def [] (ExprLiteral _ [False,True,True,False])])] = True
    checkResult _ = False

testBound :: Assertion
testBound = testResolve "testLiteral" "f a=a." checkResult
  where
    checkResult [("f",[Def [ParamBound _ [] (Identifier _ "a")]
                           (ExprBound 0)])] = True
    checkResult _ = False

testFuncall :: Assertion
testFuncall = testResolve "testLiteral" "f a=f a." checkResult
  where
    checkResult [("f",[Def [ParamBound _ [] (Identifier _ "a")]
                           (ExprFuncall (Identifier _ "f")
                                        [ExprBound 0])])] = True
    checkResult _ = False

testConcat :: Assertion
testConcat = testResolve "testLiteral" "f a=a f a." checkResult
  where
    checkResult [("f",[Def [ParamBound _ [] (Identifier _ "a")]
                           (ExprConcat
                                (ExprBound 0)
                                (ExprFuncall (Identifier _ "f")
                                             [ExprBound 0]))])] = True
    checkResult _ = False
