module TestValue
    (tests)
where

import Test.HUnit(Assertion,Test(..),assertFailure)
import Test.QuickCheck
    (Arbitrary(..),Args(..),Result(..),Testable,
     label,listOf,quickCheckWithResult,scale,stdArgs)

import Value(Value,empty,union,intersect,diff,fromList,toList,combinations)

tests :: Test
tests = TestList [
    TestCase (testProp "union" (\ v -> union v v == v)),
    TestCase (testProp "union2" (\ v -> union v empty == v)),
    TestCase (testProp "union3" (\ v w -> union v w == union w v)),
    TestCase (testProp "intersect" (\ v -> intersect v v == v)),
    TestCase (testProp "intersect2" (\ v -> intersect v empty == empty)),
    TestCase (testProp "intersect3" (\ v w -> intersect v w == intersect w v)),
    TestCase (testProp "diff" (\ v -> diff v v == empty)),
    TestCase (testProp "diff2" (\ v -> diff v empty == v)),
    TestCase (testProp "diff3" (\ v w -> diff v w == diff w v)),
    TestCase (testProp "emptyTest" emptyTest),
    TestCase (testPropWith defaultArgs{maxSize=8}
                           "combinations" combinationsTest),
    TestCase (testPropWith defaultArgs{maxSize=8}
                           "combinations2" combinationsTest2)
    ]

instance Arbitrary Value where
    arbitrary = fmap fromList (listOf (scale (flip (-) 1) arbitrary))
    shrink v | v == empty = [] | otherwise = empty : map snd (toList v)

emptyTest :: Value -> Bool
emptyTest v = (v == empty) /= (empty == intersect (fromList [(1,empty)])
                                                  (fromList [(1,v)]))

combinationsTest :: [[Int]] -> Bool
combinationsTest a = all ((== length a) . length) (combinations a)

combinationsTest2 :: [[Int]] -> Bool
combinationsTest2 a = all (and . zipWith (flip elem) a) (combinations a)

defaultArgs :: Args
defaultArgs = stdArgs{maxSize=12,chatty=False}

testProp :: Testable prop => String -> prop -> Assertion
testProp testLabel prop = testPropWith defaultArgs testLabel prop

testPropWith :: Testable prop => Args -> String -> prop -> Assertion
testPropWith args testLabel prop = do
    result <- quickCheckWithResult args (label testLabel prop)
    case result of
        Success{} -> return ()
        _ -> assertFailure (testLabel ++ ":" ++ show result)
