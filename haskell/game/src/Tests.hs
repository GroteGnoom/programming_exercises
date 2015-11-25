module Tests where

import Test.HUnit
import qualified Geometry

test1 :: Test
test1 = TestCase (assertEqual "Three equals three" (3 :: Int) (3 :: Int))

test2 :: Test
test2 = TestCase (assertEqual "Length of a vector is the nonzero value" (Geometry.len (Geometry.Vector 0 3)) 3)

tests :: Test
tests = TestList [TestLabel "FirstTest" test1, TestLabel "SecondTest" test2]

main :: IO Counts
main = runTestTT tests
