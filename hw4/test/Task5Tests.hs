module Task5Tests
  ( task5
  ) where

import Task4 (log2, mySum)
import Task5 (convertFuncOneArg, convertFuncTwoArgs)
import Test.Hspec

log2Str :: String
log2Str = "function(x1) {\n  let x2;\n  let x3 = 1;\n  let x4 = 0;\n  let x5 = 1;\n  x2 = (x4);\n"
 <> "  while (x1 > (x3)) {\n    x3 = (x3) + (x3);\n    x2 = (x2) + (x5);\n  }\n  return x2;\n}\n"

mySumStr :: String
mySumStr = "function(x1, x2) {\n  let x3;\n  x3 = x1 + x2;\n  return x3;\n}\n"

task5 :: SpecWith ()
task5 =
  describe "Testing Task5" $ do
    it "Testing func log2" $ do
      shouldBe (convertFuncOneArg log2) log2Str
    it "Testing func mySum" $ do
      shouldBe (convertFuncTwoArgs mySum) mySumStr
