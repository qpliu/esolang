module TestEncoding
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertFailure)
import Test.QuickCheck
    (Args(..),Result(..),Testable,label,quickCheckWithResult,stdArgs)

import Encoding(fromString,toString)

tests :: Test
tests = TestList [
    TestCase (testEncoding "testEncoding" (\s -> s == toString (fromString s)))
    ]

testEncoding :: Testable prop => String -> prop -> Assertion
testEncoding testLabel prop = do
    result <- quickCheckWithResult stdArgs{chatty=False} (label testLabel prop)
    case result of
        Success{} -> return ()
        _ -> assertFailure (testLabel ++ ":" ++ show result)
