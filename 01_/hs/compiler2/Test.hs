import Test.HUnit
    (Test(..),runTestTT)

import qualified TestParse
import qualified TestResolve
import qualified TestCodeGen

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestParse" TestParse.tests,
                         TestLabel "TestResolve" TestResolve.tests,
                         TestLabel "TestCodeGen" TestCodeGen.tests])
    return ()
