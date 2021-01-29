module Task4Tests
  ( task4
  ) where

import Task4 (convertToHalyavaScript, interpretOneArg, interpretTwoArgs, log2, mySum)
import Test.Hspec (SpecWith, describe, it, shouldBe)

task4 :: SpecWith ()
task4 =
  describe "Testing Task4" $ do
    it "Testing func log2" $ do
      shouldBe (interpretOneArg (1 :: Int) log2) $ convertToHalyavaScript "0"
      shouldBe (interpretOneArg (38 :: Int) log2) $ convertToHalyavaScript "6"
      shouldBe (interpretOneArg (38 :: Double) log2) $ convertToHalyavaScript "6"
      shouldBe (interpretOneArg ((2 ^ (32 :: Int)) :: Int) log2) $ convertToHalyavaScript "32"
    it "Testing func mySum" $ do
      shouldBe (interpretTwoArgs "I love" " fp!" mySum) $ convertToHalyavaScript "I love fp!"
      shouldBe (interpretTwoArgs (19 :: Int) (19 :: Int) mySum) $ convertToHalyavaScript "38"
      shouldBe (interpretTwoArgs (2.3 :: Double) (7 :: Double) mySum) $ convertToHalyavaScript "9.3"
