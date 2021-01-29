{-# LANGUAGE TypeFamilies #-}

module Task5
  ( convertFuncOneArg
  , convertFuncTwoArgs
  ) where

import Data.Functor.Const (Const)
import Task4 (HalyavaScript (..))
import Task4Utils (addTabs)

data Storage v =
  Storage
    { indToStr :: Int -> String
    }

getOp :: Storage v1 -> Storage v2 -> [Char] -> Storage v3
getOp v1 v2 op = Storage $ \ind -> indToStr v1 ind <> " " <> op <> " " <> indToStr v2 ind

instance HalyavaScript Storage where
  type Variable Storage v = Const String v

  var @= v = Storage $ \ind -> indToStr var ind <> " = " <> indToStr v ind <> ";\n"

  sVariable var = Storage $ \ind -> "(" <> indToStr var ind <> ")"

  sWithVar var body = Storage $ \ind -> "let x" <> show ind <> " = " <> show var <> ";\n"
    <> indToStr (body $ Storage $ const $ "x" <> show ind) (ind + 1)

  v1 # v2   = Storage $ \ind -> indToStr v1 ind <> indToStr v2 ind

  v1 @> v2  = getOp v1 v2 ">"
  v1 @< v2  = getOp v1 v2 "<"
  v1 @>= v2 = getOp v1 v2 ">="
  v1 @<= v2 = getOp v1 v2 "<="
  v1 @== v2 = getOp v1 v2 "==="
  v1 @/= v2 = getOp v1 v2 "!=="

  v1 @+ v2  = getOp v1 v2 "+"
  v1 @- v2  = getOp v1 v2 "-"

  v1 @* v2 = getOp v1 v2 "*"

  sIf cond body1 body2 = Storage $ \ind -> "if (" <> indToStr cond ind <> ") {\n"
    <> addTabs (indToStr body1 ind) <> "\n} else {\n"
    <> addTabs (indToStr body2 ind) <> "\n}\n"

  sWhile cond body = Storage $ \ind -> "while (" <> indToStr cond ind <> ") {\n"
    <> addTabs (indToStr body ind) <> "\n}\n"

  sFun1 body _ = Storage $ \ind -> "function(x" <> show ind <> ") {\n"
    <> addTabs ("let x" <> show (ind + 1) <> ";\n"
      <> indToStr (body (Storage $ const $ "x" <> show ind) (Storage $ const $ "x" <> show (ind + 1)))
        (ind + 2)  <> "return x" <> show (ind + 1) <> ";")<> "\n}\n"

  sFun2 body _ _ = Storage $ \ind ->
    "function(x" <> show ind <> ", x" <> show (ind + 1) <> ") {\n"
    <> addTabs ("let x" <> show (ind + 2) <> ";\n"
      <> indToStr (body (Storage $ const $ "x" <> show ind)
                    (Storage $ const $ "x" <> show (ind + 1))
                    (Storage $ const $ "x" <> show (ind + 2)))
         (ind + 3) <> "return x" <> show (ind + 2) <> ";") <> "\n}\n"

convertFuncOneArg :: (Storage v -> Storage v)  -> String
convertFuncOneArg f = indToStr (f $ Storage $ const $ "") 1

convertFuncTwoArgs :: (Storage v -> Storage v -> Storage v) -> String
convertFuncTwoArgs f = indToStr (f (Storage $ const $ "") (Storage $ const $ "")) 1
