module Task4Tests
  ( task4
  ) where

import Task4 (log2, interpretOneArg, interpretTwoArgs, mySum, convertToHalyavaScript)
import Test.Hspec (SpecWith, describe, it, shouldBe)

task4 :: SpecWith ()
task4 =
  describe "checks 'HS' functions results" $ do
    it "checking the 'log2' function result" $ do
      interpretOneArg (1 :: Int) log2 `shouldBe` convertToHalyavaScript "0"
      interpretOneArg (2 :: Int) log2 `shouldBe` convertToHalyavaScript "1"
      interpretOneArg (4 :: Int) log2 `shouldBe` convertToHalyavaScript "2"
      interpretOneArg (5 :: Int) log2 `shouldBe` convertToHalyavaScript "3"
      interpretOneArg (8 :: Int) log2 `shouldBe` convertToHalyavaScript "3"
      interpretOneArg (17 :: Int) log2 `shouldBe` convertToHalyavaScript "5"

    it "checking the 'concatenate' function result" $ do
      interpretTwoArgs "kes" "pop" mySum `shouldBe` convertToHalyavaScript "kespop"
      interpretTwoArgs (1 :: Int) (1 :: Int) mySum `shouldBe` convertToHalyavaScript "2"
      interpretTwoArgs (1 :: Double) (1 :: Double) mySum `shouldBe` convertToHalyavaScript "2.0"
