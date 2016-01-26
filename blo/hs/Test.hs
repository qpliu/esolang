import Test.HUnit
    (Test(..),runTestTT)

import qualified TestLLVMCodeGen

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestLLVMCodeGen" TestLLVMCodeGen.tests])
    return ()
