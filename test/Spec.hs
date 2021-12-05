import Test.Tasty
import Test.Tasty.HUnit

import Challenge

main = defaultMain tests

makeTest :: (Challenge, Int) -> TestTree
makeTest (challenge, day) =
    testGroup ("Day " ++ show day) 
        [ testCase "Part 1" $ (_part1 challenge) (_example challenge) @?= (_exampleResult1 challenge)
        , testCase "Part 2" $ (_part2 challenge) (_example challenge) @?= (_exampleResult2 challenge)
        ]

tests :: TestTree
tests = testGroup "Examples" (fmap makeTest (zip challenges [1..]) )