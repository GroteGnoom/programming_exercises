module Tests where

import Test.HUnit

test1 = TestCase (assertEqual "Three equals three" 3 3)
test2 = TestCase (assertEqual "Will fail" 4 3)


tests = TestList [TestLabel "FirstTest" test1, TestLabel "SecondTest" test2]
