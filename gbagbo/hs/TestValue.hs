module TestValue
    (check)
where

import Test.QuickCheck(quickCheck)
import Test.QuickCheck.Arbitrary(Arbitrary(arbitrary))
import Test.QuickCheck.Gen(elements)

import Value(Value,empty,union,intersect,diff,fromList)

instance Arbitrary Value where
    -- Replace with actually generated values
    arbitrary = elements [empty,fromList [(1,empty)],fromList [(2,empty)],
                          fromList [(3,fromList [(4,empty)])],
                          fromList [(5,fromList [(6,empty)]),(7,empty)],
                          fromList [(8,fromList [(9,empty),(10,fromList [(11,empty)])])]]

check :: IO ()
check = sequence_ quickChecks

quickChecks :: [IO ()]
quickChecks = [
    quickCheck (\ v -> union v v == v),
    quickCheck (\ v -> union v empty == v),
    quickCheck (\ v -> intersect v v == v),
    quickCheck (\ v -> diff v v == empty),
    quickCheck (\ v -> (intersect (fromList [(1,empty)]) (fromList [(1,v)]) == empty) /= (v == empty))
    ]
