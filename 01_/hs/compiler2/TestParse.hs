module TestParse
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertFailure)

import Ast(Identifier(..),PartialDef(..),Param(..),Unparsed(..))
import Compile(compile)
import Parse(parse)

tests :: Test
tests = TestList [
    TestCase testSimple,
    TestCase testTwo,
    TestCase testComment,
    TestCase testLiteral,
    TestCase testLiteral2,
    TestCase testBoundParameter,
    TestCase testLiteralParameter,
    TestCase testIgnoreParameter,
    TestCase testImplicitParameter,
    TestCase testTwoParameters
    ]

testParse :: String -> String -> ([PartialDef] -> Bool) -> Assertion
testParse label code checkResult =
    either (assertFailure . ((label ++ ":") ++) . show)
           (\ result ->
                if checkResult result then return () else assertFailure label)
           (compile (parse "(test)" code))

testSimple :: Assertion
testSimple = testParse "testSimple" "f=." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") [] []] = True
    checkResult _ = False

testTwo :: Assertion
testTwo = testParse "testTwo" "f=.g=." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") [] [],
                 PartialDef (Identifier _ "g") [] []] = True
    checkResult _ = False

testComment :: Assertion
testComment = testParse "testComment" "f==.g=.\n1=1." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamIgnored _ [True]]
                            [UnparsedLiteral _ [True]]] = True
    checkResult _ = False

testLiteral :: Assertion
testLiteral = testParse "testLiteral" "f=0110." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") []
                            [UnparsedLiteral _ [False,True,True,False]]] = True
    checkResult _ = False

testLiteral2 :: Assertion
testLiteral2 = testParse "testLiteral2" "f=0101_ 011." checkResult
  where
    checkResult [PartialDef (Identifier _ "f") []
                            [UnparsedLiteral _ [False,True,False,True],
                             UnparsedLiteral _ [False,True,True]]] = True
    checkResult _ = False

testBoundParameter :: Assertion
testBoundParameter = testParse "testBoundParameter" "f 01a=10a." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamBound _ [False,True] (Identifier _ "a")]
                            [UnparsedLiteral _ [True,False],
                             UnparsedIdentifier (Identifier _ "a")]] = True
    checkResult _ = False

testLiteralParameter :: Assertion
testLiteralParameter =
    testParse "testImplicitParameter" "f 01_=f 01." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamLiteral _ [False,True]]
                            [UnparsedIdentifier (Identifier _ "f"),
                             UnparsedLiteral _ [False,True]]] = True
    checkResult _ = False

testIgnoreParameter :: Assertion
testIgnoreParameter =
    testParse "testImplicitParameter" "f 01.=f 01." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamIgnored _ [False,True]]
                            [UnparsedIdentifier (Identifier _ "f"),
                             UnparsedLiteral _ [False,True]]] = True
    checkResult _ = False

testImplicitParameter :: Assertion
testImplicitParameter =
    testParse "testImplicitParameter" "f 01=f 01." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamIgnored _ [False,True]]
                            [UnparsedIdentifier (Identifier _ "f"),
                             UnparsedLiteral _ [False,True]]] = True
    checkResult _ = False

testTwoParameters :: Assertion
testTwoParameters = testParse "testTwoParameters" "f 01a b=10a b." checkResult
  where
    checkResult [PartialDef (Identifier _ "f")
                            [ParamBound _ [False,True] (Identifier _ "a"),
                             ParamBound _ [] (Identifier _ "b")]
                            [UnparsedLiteral _ [True,False],
                             UnparsedIdentifier (Identifier _ "a"),
                             UnparsedIdentifier (Identifier _ "b")]] = True
    checkResult _ = False
