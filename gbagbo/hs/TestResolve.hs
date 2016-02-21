module TestResolve
    (tests)
where

import Control.Monad(unless)
import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)

import Ast(Identifier(..),Def(..),Expr(..))
import Compile(compile)
import Parse(parse)
import Resolve(resolve)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testBag,
    TestCase testBag2,
    TestCase testBag3,
    TestCase testBag4,
    TestCase testBag5,
    TestCase testBinop,
    TestCase testBinop2,
    TestCase testBinop3,
    TestCase testBinop4,
    TestCase testBinop5,
    TestCase testBinop6,
    TestCase testBinop7,
    TestCase testBinop8,
    TestCase testParens,
    TestCase testParens2,
    TestCase testFuncall
    ]

testResolve :: String -> String -> ([(String,Def)] -> Bool) -> Assertion
testResolve label code checkResult =
    either (assertFailure . ((label ++ ":") ++) . show)
           (flip unless (assertFailure label) . checkResult)
           (compile (parse "(test)" code >>= resolve))

testResolveError :: String -> String -> String -> Assertion
testResolveError label code expected =
    either (assertEqual label expected . show)
           (const (assertFailure label))
           (compile (parse "(test)" code >>= resolve))

testSimple :: Assertion
testSimple = testResolve "testSimple" "f a=a." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 1 (ExprBound _ "a" 0))] = True
    checkResult _ = False

testBag :: Assertion
testBag = testResolve "testBag" "f=[]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 0 (ExprBag _ []))] = True
    checkResult _ = False

testBag2 :: Assertion
testBag2 = testResolve "testBag2" "f=[[]]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 0
                        (ExprBag _ [(1,ExprBag _ [])]))] = True
    checkResult _ = False

testBag3 :: Assertion
testBag3 = testResolve "testBag3" "f=[2×[]]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 0
                        (ExprBag _ [(2,ExprBag _ [])]))] = True
    checkResult _ = False

testBag4 :: Assertion
testBag4 = testResolve "testBag4" "f=[2×[][]]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 0
                        (ExprBag _ [(2,ExprBag _ []),
                                    (1,ExprBag _ [])]))] = True
    checkResult _ = False

testBag5 :: Assertion
testBag5 = testResolve "testBag5" "f a=[a]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 1
                        (ExprBag _ [(1,ExprBound _ "a" 0)]))] = True
    checkResult _ = False

testBinop :: Assertion
testBinop = testResolve "testBinop" "f a b=a|b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprUnion _ (ExprBound _ "a" 0)
                                     (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop2 :: Assertion
testBinop2 = testResolve "testBinop2" "f a b=a∪b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprUnion _ (ExprBound _ "a" 0)
                                     (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop3 :: Assertion
testBinop3 = testResolve "testBinop3" "f a b=a&b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprIntersect _ (ExprBound _ "a" 0)
                                         (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop4 :: Assertion
testBinop4 = testResolve "testBinop4" "f a b=a∩b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprIntersect _ (ExprBound _ "a" 0)
                                         (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop5 :: Assertion
testBinop5 = testResolve "testBinop5" "f a b=a^b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprDiff _ (ExprBound _ "a" 0)
                                    (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop6 :: Assertion
testBinop6 = testResolve "testBinop6" "f a b=a△b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprDiff _ (ExprBound _ "a" 0)
                                    (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop7 :: Assertion
testBinop7 = testResolve "testBinop7" "f a b=a⊖b." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprDiff _ (ExprBound _ "a" 0)
                                    (ExprBound _ "b" 1)))] = True
    checkResult _ = False

testBinop8 :: Assertion
testBinop8 = testResolve "testBinop8" "f a b=a^b|a." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprUnion _ (ExprDiff _ (ExprBound _ "a" 0)
                                                 (ExprBound _ "b" 1))
                                     (ExprBound _ "a" 0)))] = True
    checkResult _ = False

testParens :: Assertion
testParens = testResolve "testParens" "f a b=([])." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2 (ExprBag _ []))] = True
    checkResult _ = False

testParens2 :: Assertion
testParens2 = testResolve "testParens2" "f a b=a^(b|a)." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 2
                        (ExprDiff _ (ExprBound _ "a" 0)
                                    (ExprUnion _ (ExprBound _ "b" 1)
                                                 (ExprBound _ "a" 0))))] = True
    checkResult _ = False

testFuncall :: Assertion
testFuncall = testResolve "testFuncall" "f a=g*a.g a=[]." checkResult
  where
    checkResult [(_,Def (Identifier _ "f") 1
                        (ExprFuncall _ (Def (Identifier _ "g") _ _)
                                            [(True,ExprBound _ "a" 0)])),
                 (_,Def (Identifier _ "g") 1 (ExprBag _ []))] = True
    checkResult _ = False
