module Block2.Task1
  ( -- * Types
    ArithmeticError(..)
  , Expr(..)

   -- * Functions
  , eval
  ) where

-- | Data type for arithmetic error
data ArithmeticError
  = DividingByZero
  | NegativePow
  deriving (Show, Eq)

-- | Data type for expressions
data Expr
  = Const Int
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  deriving (Show, Eq)

-- | Safely converts the expression to an integer result
eval :: Expr -> Either ArithmeticError Int
eval (Const a) = Right a
eval (Add a b) = (+) <$> eval a <*> eval b
eval (Sub a b) = (-) <$> eval a <*> eval b
eval (Mul a b) = (*) <$> eval a <*> eval b
eval (Div a b) = eval b >>= \evalB -> if evalB == 0 then Left DividingByZero
  else fmap (`div` evalB) (eval a)
eval (Pow a b) = eval b >>= \evalB -> if evalB < 0 then Left NegativePow
  else fmap (^ evalB) (eval a)
