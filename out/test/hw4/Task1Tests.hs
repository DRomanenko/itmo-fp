module Task1Tests
  ( pointGenerator
  , task1
  ) where

import Criterion.Main (bench, bgroup, defaultMain, nf)
import Task1
import Test.Hspec (SpecWith, describe, it, shouldBe)
import Test.QuickCheck (Gen, choose, generate, vectorOf)

pointGenerator :: Int -> Gen [Point]
pointGenerator size = do
  let genPoint = do
        x' <- choose (-size, size)
        y' <- choose (-size, size)
        return (Point x' y')
  list <- vectorOf size genPoint
  return list

task1 :: SpecWith ()
task1 =
  describe "Testing Task1" $ do
    let point1 = Point 0  0
    let point2 = Point 0  38
    let point3 = Point 38 0
    let point4 = Point 38 38
    it "Testing op plus" $ do
      shouldBe (point1 `plus` point4) (Point 38 38)
    it "Testing op minus" $ do
      shouldBe (point1 `minus` point4) (Point (-38) (-38))
    it "Testing func scalarProduct" $ do
      shouldBe (point2 `scalarProduct` point3) 0
    it "Testing func crossProduct" $ do
      shouldBe (point2 `crossProduct` point3) (-1444)
    it "Testing func fast perimeter" $ do
      shouldBe (perimeter [point1, point3, point4, point2]) (152)
    it "Testing func slow perimeter" $ do
      shouldBe (perimeterSlow [point1, point3, point4, point2]) (152)
    it "Testing func fast doubleArea" $ do
      shouldBe (doubleArea [point1, point3, point4, point2]) (2888)
    it "Testing func slow doubleArea" $ do
      shouldBe (doubleAreaSlow [point1, point3, point4, point2]) (2888)
    it "Testing performance perimeter" $ do
      pointsForP <- generate $ pointGenerator (10 ^ (7 :: Int))
      defaultMain
        [ bgroup "slow perimeter" [ bench "10^7"  $ nf perimeterSlow pointsForP ]
        , bgroup "fast perimeter" [ bench "10^7"  $ nf perimeter pointsForP     ]
        ]
    it "Testing performance area" $ do
      pointsForD <- generate $ pointGenerator (10 ^ (7 :: Int))
      defaultMain
        [ bgroup "slow doubleArea" [ bench "10^7"  $ nf doubleAreaSlow pointsForD ]
        , bgroup "fast doubleArea" [ bench "10^7"  $ nf doubleArea pointsForD     ]
        ]
