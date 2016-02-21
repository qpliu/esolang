import Test.HUnit
    (Test(..),runTestTT)

import qualified TestEncoding
import qualified TestEval
import qualified TestParse
import qualified TestResolve
import qualified TestValue

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestParse" TestParse.tests,
                         TestLabel "TestResolve" TestResolve.tests,
                         TestLabel "TestValue" TestValue.tests,
                         TestLabel "TestEncoding" TestEncoding.tests,
                         TestLabel "TestEval" TestEval.tests])
    return ()
