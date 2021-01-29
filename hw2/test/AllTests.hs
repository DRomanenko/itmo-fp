module AllTests
  ( hTestTree
  ) where

import Block1.Task1 (stringSum)
import Block2.Task1

import Test.Tasty
import Test.Tasty.Hspec

allTests :: Spec
allTests = do
  describe "Block1.Task1" $
    context "stringSum-Just" $ do
      let test1 = "12345678"
      let test2 = "12345 123"
      let test3 = " 123 12345 "
      let test4 = "123 12345 "
      let test5 = "\n123\t12345 "
      let test6 = "-123 12345"
      let test7 = "123 321 231"
      it test1 $
        shouldBe (stringSum test1) (Just 12345678)
      it test2 $
        shouldBe (stringSum test2) (Just 12468)
      it test3 $
        shouldBe (stringSum test3) (Just 12468)
      it test4 $
        shouldBe (stringSum test4) (Just 12468)
      it "\\n123\\t12345 " $
        shouldBe (stringSum test5) (Just 12468)
      it test6 $
        shouldBe (stringSum test6) (Just 12222)
      it test7 $
        shouldBe (stringSum test7) (Just 675)
  describe "Block1.Task1" $
    context "stringSum-Nothing" $ do
      let test1 = "12345678h"
      let test2 = "12345+123"
      let test3 = "xyz"
      let test4 = "-123-12345"
      it test1 $
        shouldBe (stringSum test1) Nothing
      it test2 $
        shouldBe (stringSum test2) Nothing
      it test3 $
        shouldBe (stringSum test3) Nothing
      it test4 $
        shouldBe (stringSum test4) Nothing
  describe "Block2.Task1" $
    context "Operations that return Int" $ do
      let a = 12345678
      let b = 12
      let c = -2
      let zero = 0
      it ("Const " ++ show a) $
        shouldBe (eval (Const a)) (Right a)
      it "(a + b) / c" $
        shouldBe (eval (Div (Add (Const a) (Const b)) (Const c))) (Right (div (a + b) c))
      it "(a * zero) / (b + zero)" $
        shouldBe
        (eval (Div (Mul (Const a) (Const zero)) (Add (Const b) (Const zero))))
        (Right (div (a * zero) (b + zero)))
      it "b * (b ^ b - c)" $
        shouldBe
        (eval (Mul (Const b) (Sub (Pow (Const b) (Const b)) (Const c))))
        (Right (b * (b ^ b - c)))
  describe "Block2.Task1" $
    context "Operations that return ArithmeticError" $ do
      let a = 38
      let negative = -1
      let zero = 0
      it "a / zero" $
        shouldBe (eval (Div (Const a) (Const zero))) (Left DividingByZero)
      it "a ^ negative" $
        shouldBe (eval (Pow (Const a) (Const negative))) (Left NegativePow)

hTestTree :: IO TestTree
hTestTree = testSpec "All Tests" allTests
