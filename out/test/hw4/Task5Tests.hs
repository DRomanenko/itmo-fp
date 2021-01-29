module Task5Tests
  ( task5
  ) where

import Test.Hspec

import Task4 (log2, mySum)
import Task5 (convertFuncOneArg, convertFuncTwoArgs)

log2Str :: String
log2Str = "function(x0) {\n  let x1;\n  let x2 = 1;\n  let x3 = 1;\n"
  <> "  let x4 = 0;\n  x1 = (x4);\n  while (x0 > (x2)) {\n"
  <> "    x2 = (x2) + (x2);\n    x1 = (x1) + (x3);\n  }\n  return x1;\n}\n"

mySumStr :: String
mySumStr = "function(x0, x1) {\n  let x2;\n  x2 = x0 + x1;\n  return x2;\n}\n"


task5 :: Spec
task5 =
  describe "Testing Task5" $ do
    it "checking the 'log2' function representation" $ do
      printFunc log2 `shouldBe` log2Str

    it "checking the 'sum' function representation" $ do
      printFunc2 mySum `shouldBe` mySumStr
