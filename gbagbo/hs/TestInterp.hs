module TestInterp
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertEqual,assertFailure)
import Test.QuickCheck
    (Args(..),Result(..),Testable,label,quickCheckWithResult,stdArgs)

import Ast(Def(..),Expr)
import Compile(compile)
import Encoding(fromString,toString)
import Eval(eval)
import Parse(parse)
import Resolve(resolve)
import Value(empty)

tests :: Test
tests = TestList [
    TestCase testHello,
    TestCase testCat
    ]

getMain :: String -> String -> String -> IO Def
getMain testLabel mainFunc code = do
    defs <- either ((>> undefined) . assertFailure
                                   . ((testLabel ++ ":") ++) . show)
                   return
                   (compile (parse "(test)" code >>= resolve))
    maybe (assertFailure (testLabel ++ ": " ++ mainFunc) >> undefined)
          return
          (lookup mainFunc defs)

testProg :: String -> String -> String -> String -> String -> Assertion
testProg testLabel mainFunc code input expected = do
    Def _ nargs expr <- getMain testLabel mainFunc code
    assertEqual testLabel expected
                (toString (eval (take nargs (fromString input:repeat empty))
                                expr))

testOutput :: Testable prop => String -> String -> String
           -> ((String -> String) -> prop)
           -> Assertion
testOutput testLabel mainFunc code prop = do
    Def _ nargs expr <- getMain testLabel mainFunc code
    let test = prop (toString . flip eval expr . take nargs
                              . (:repeat empty) . fromString)
    result <-
        quickCheckWithResult stdArgs{chatty=False} (label testLabel test)
    case result of
        Success{} -> return ()
        _ -> assertFailure (testLabel ++ ":" ++ show result)

testHello :: Assertion
testHello = testProg "testHello" "hello"
    ("hello = 0 1 0 0 1 0 0 0 0 1 1 0 0 1 0 1 0 1 1 0 1 1 0 0 0 1 1 0 1 1 " ++
     "        0 0 0 1 1 0 1 1 1 1 0 0 1 0 0 0 0 0 0 1 1 1 0 1 1 1 0 1 1 0 " ++
     "        1 1 1 1 0 1 1 1 0 0 1 0 0 1 1 0 1 1 0 0 0 1 1 0 0 1 0 0 0 0 " ++
     "        1 0 0 0 0 1 0 0 0 0 1 0 1 0 []. " ++
     "0 x = [x]." ++
     "1 x = [[]x].")
    "" "Hello world!\n"

testCat :: Assertion
testCat = testOutput "testCat" "cat"
    "cat x = x."
    (\ cat input -> cat input == input)
