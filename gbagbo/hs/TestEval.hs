module TestEval
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertFailure)
import Test.QuickCheck
    (Args(..),Result(..),Testable,label,quickCheckWithResult,stdArgs)

import Ast(Def(..),Expr)
import Compile(compile)
import Eval(eval)
import Parse(parse)
import Resolve(resolve)
import Value(Value,empty,union,intersect,diff,fromList,toList)
import TestValue()

tests :: Test
tests = TestList [
    TestCase (testUnary "id" "f a=a." id),
    TestCase (testBinary "union" "f a b=a∪b." union),
    TestCase (testBinary "intersect" "f a b=a∩b." intersect),
    TestCase (testBinary "diff" "f a b=a△b." diff),
    TestCase (testUnary "bag" "f a=[a]." (fromList . (:[]) . ((,) 1))),
    TestCase (testUnary "bag2" "f a=[2×a]." (fromList . (:[]) . ((,) 2))),
    TestCase (testUnary "funcall" "f a=g a.g a=a." id),
    TestCase (testUnary "map" "f a=g*a.g a=a." (fromList . toList)),
    TestCase (testUnary "map2" "f a=g*a.g a=[]."
                               (fromList . map (fmap (const empty)) . toList))
    ]

defaultArgs :: Args
defaultArgs = stdArgs{maxSize=6,chatty=False}

getExpr :: String -> String -> IO Expr
getExpr testLabel code = do
    ((_,Def _ _ expr):_) <-
        either ((>> undefined) . assertFailure
                               . ((testLabel ++ ":") ++) . show)
               return
               (compile (parse "(test)" code >>= resolve))
    return expr

testExpr :: Testable prop => String -> String -> (Expr -> prop) -> Assertion
testExpr testLabel code prop = do
    expr <- getExpr testLabel code
    result <- quickCheckWithResult defaultArgs (label testLabel (prop expr))
    case result of
        Success{} -> return ()
        _ -> assertFailure (testLabel ++ ":" ++ show result)

testUnary :: String -> String -> (Value -> Value) -> Assertion
testUnary testLabel code fn =
    testExpr testLabel code (\ expr v -> eval [v] expr == fn v)

testBinary :: String -> String -> (Value -> Value -> Value) -> Assertion
testBinary testLabel code fn =
    testExpr testLabel code (\ expr v w -> eval [v,w] expr == fn v w)
