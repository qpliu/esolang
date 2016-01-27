import Test.HUnit
    (Test(..),runTestTT)

import qualified TestLLVMCodeGen
import qualified TestLLVMRuntime

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestLLVMCodeGen" TestLLVMCodeGen.tests,
                         TestLabel "TestLLVMRuntime" TestLLVMRuntime.tests])
    return ()
