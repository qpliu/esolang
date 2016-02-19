import Test.HUnit
    (Test(..),runTestTT)

import qualified TestParse
import qualified TestResolve

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestParse" TestParse.tests,
                         TestLabel "TestResolve" TestResolve.tests])
    return ()
