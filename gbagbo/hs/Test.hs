import System.IO(stderr)
import Test.HUnit
    (Test(..),runTestText,putTextToHandle)

import qualified TestEncoding
import qualified TestEval
import qualified TestInterp
import qualified TestParse
import qualified TestResolve
import qualified TestValue

main :: IO ()
main = do
    runTestText (putTextToHandle stderr False)
                (TestList [TestLabel "TestParse" TestParse.tests,
                           TestLabel "TestResolve" TestResolve.tests,
                           TestLabel "TestValue" TestValue.tests,
                           TestLabel "TestEncoding" TestEncoding.tests,
                           TestLabel "TestEval" TestEval.tests,
                           TestLabel "TestInterp" TestInterp.tests])
    return ()
