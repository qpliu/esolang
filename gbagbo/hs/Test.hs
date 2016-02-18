import Test.HUnit
    (Test(..),runTestTT)

import qualified TestParse

main :: IO ()
main = do
    runTestTT (TestList [TestLabel "TestParse" TestParse.tests])
    return ()
