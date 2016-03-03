module TestParse
    (tests)
where

import Control.Monad(unless)
import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)

import Ast(Identifier(..),PartialDef(..),Token(..))
import Compile(compile)
import Parse(parse)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testTwo,
    TestCase testComment,
    TestCase testError
    ]

testParse :: String -> String -> ([PartialDef] -> Bool) -> Assertion
testParse label code checkResult =
    either (assertFailure . ((label ++ ":") ++) . show)
           (flip unless (assertFailure label) . checkResult)
           (compile (parse "(test)" code))

testParseError :: String -> String -> String -> Assertion
testParseError label code expected =
    either (assertEqual label expected . show)
           (const (assertFailure label))
           (compile (parse "(test)" code))

testSimple :: Assertion
testSimple = testParse "testSimple" "f a=[a]." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") [Identifier _ "a"]
                            [Token _ "[",Token _ "a",Token _ "]" ] _] = True
    checkResult _ = False

testTwo :: Assertion
testTwo = testParse "testTwo" "f a=g a.g a=a." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") [Identifier _ "a"]
                            [Token _ "g",Token _ "a"] _,
                 PartialDef (Identifier _ "g") [Identifier _ "a"]
                            [Token _ "a"] _] = True
    checkResult _ = False

testComment :: Assertion
testComment = testParse "testComment" "f == a=g a.g\n a=a." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") [Identifier _ "a"]
                            [Token _ "a"] _] = True
    checkResult _ = False

testError :: Assertion
testError = testParseError "testError" "f a = a" "(test):1:8: \".\""
