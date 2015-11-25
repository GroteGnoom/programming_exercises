module Tests where

import Test.HUnit
import qualified Geometry as Geo

vec1 :: Test
vec1 = TestCase ( assertEqual "Length of a vector is the nonzero value"
                              (Geo.len (Geo.Vec 0 3))
                              3)

vec2 :: Test
vec2 = TestCase ( assertEqual "Vec1 - Vec1 = 0vec"
                              ( Geo.vSub ( Geo.Vec 4 3)
                                         ( Geo.Vec 4 3))
                              ( Geo.Vec 0 0))

vec3 :: Test
vec3 = TestCase ( assertEqual "len Vec1 - Vec1 = 0"
                              (Geo.len (Geo.vSub (Geo.Vec 4 3)
                                                 (Geo.Vec 4 3)))
                              0)

seg1 :: Test
seg1 = TestCase ( assertEqual "len seg1 seg1 = 0"
                              (Geo.len (Geo.Seg (Geo.Vec 4 3)
                                                (Geo.Vec 4 3)))
                              0)

seg2 :: Test
seg2 = TestCase ( assertEqual "cross segments"
                              (Geo.intersect (Geo.Seg (Geo.Vec 0 0)
                                                          (Geo.Vec 0 2))
                                             (Geo.Seg (Geo.Vec 1 (-1))
                                                          (Geo.Vec 1 1)))
                              True)

poly1 :: Test
poly1 = TestCase ( assertEqual "intersecting polygons"
                               (Geo.intersect (Geo.Poly   [ Geo.Vec 0 0
                                                          , Geo.Vec 0 2
                                                          , Geo.Vec 2 2])
                                              (Geo.Poly   [ Geo.Vec 1 (-1)
                                                          , Geo.Vec 1 1
                                                          , Geo.Vec (-1) (-1)]))
                              True)

tests :: Test
tests = TestList [vec1, vec2, vec3,seg1, seg2, poly1]

main :: IO Counts
main = runTestTT tests
