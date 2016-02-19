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
    TestCase testSimple
    ]

testResolve :: String -> String -> ([Def] -> Bool) -> Assertion
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
    checkResult [Def (Identifier _ "f") 1 (ExprBound _ "a" 0)] = True
    checkResult _ = False
