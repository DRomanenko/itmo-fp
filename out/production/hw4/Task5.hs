{-# LANGUAGE TypeFamilies #-}

module Task5
  ( convertFuncOneArg
  , convertFuncTwoArgs
  ) where

import Data.Functor.Const (Const)

import Task4 (HalyavaScript (..))

newtype Storage a =
  Storage
    { toString :: Int -> String
    }

instance HalyavaScript Storage where
  type Variable Storage a = Const String a

  var @= v = Storage $ \n -> toString var n <> " = " <> toString v n <> ";\n"

  sVariable var = Storage $ \n -> "(" <> toString var n <> ")"

  sWithVar var body = Storage $ \n -> "let x" <> show n <> " = " <> show var <> ";\n"
    <> toString (body $ Storage $ const $ "x" <> show n) (n + 1)


  line1 # line2 = Storage $ \n -> toString line1 n <> toString line2 n

  a @> b = Storage $ \n -> toString a n <> " > " <> toString b n
  a @< b = Storage $ \n -> toString a n <> " < " <> toString b n
  a @>= b = Storage $ \n -> toString a n <> " >= " <> toString b n
  a @<= b = Storage $ \n -> toString a n <> " <= " <> toString b n
  a @== b = Storage $ \n -> toString a n <> " === " <> toString b n
  a @/= b = Storage $ \n -> toString a n <> " !== " <> toString b n

  a @+ b = Storage $ \n -> toString a n <> " + " <> toString b n
  a @- b = Storage $ \n -> toString a n <> " - " <> toString b n

  a @* b = Storage $ \n -> toString a n <> " * " <> toString b n

  sIf cond body1 body2 = Storage $ \n -> "if (" <> toString cond n <> ") {\n"
    <> tabulator (toString body1 n)
    <> "\n} else {\n"
    <> tabulator (toString body2 n)
    <> "\n}\n"

  sWhile cond body = Storage $ \n -> "while (" <> toString cond n <> ") {\n"
    <> tabulator (toString body n) <> "\n}\n"

  sFun1 body _ = Storage $ \n -> "function(x" <> show n <> ") {\n"
    <> tabulator ("let x" <> show (n + 1) <> ";\n"
      <> toString (body (Storage $ const $ "x" <> show n)
        $ Storage $ const $ "x" <> show (n + 1))
        (n + 2)
      <> "return x" <> show (n + 1) <> ";"
      )<> "\n}\n"

  sFun2 body _ _ = Storage $ \n ->
    "function(x" <> show n <> ", x" <> show (n + 1) <> ") {\n"
    <> tabulator ("let x" <> show (n + 2) <> ";\n"
      <> toString
          (body (Storage $ const $ "x" <> show n)
                (Storage $ const $ "x" <> show (n + 1))
                (Storage $ const $ "x" <> show (n + 2)))
          (n + 3)
      <> "return x" <> show (n + 2) <> ";"
      ) <> "\n}\n"

tabulator :: String  -> String
tabulator = tail . concatMap ("\n  " <>) . lines

convertFuncOneArg :: (Storage a -> Storage a)  -> String
convertFuncOneArg f = toString (f $ Storage $ const $ "") 0

convertFuncTwoArgs :: (Storage a -> Storage a -> Storage a) -> String
convertFuncTwoArgs f = toString (f (Storage $ const $ "") (Storage $ const $ "")) 0
