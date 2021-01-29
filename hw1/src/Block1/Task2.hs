{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  (
    -- * Type with ops
    Nat(..)

    -- * Function
  , isEven
  ) where

data Nat
  = Z
  | S Nat
  deriving (Show)

instance Enum Nat where
  fromEnum :: Nat -> Int
  fromEnum Z     = 0
  fromEnum (S x) = fromEnum x + 1

  toEnum :: Int -> Nat
  toEnum x
    | x < 0     = error "The natural number must be positive!"
    | x == 0    = Z
    | otherwise = S $ toEnum $ x - 1

instance Num Nat where
  (+) :: Nat -> Nat -> Nat
  (+) Z x       = x
  (+) x1 (S x2) = S (x1 + x2)
  (+) (S x1) x2 = S (x1 + x2)

  (*) :: Nat -> Nat -> Nat
  (*) Z _       = Z
  (*) x1 (S x2) = x1 * x2 + x1
  (*) (S x1) x2 = x1 * x2 + x2

  (-) :: Nat -> Nat -> Nat
  (-) Z _           = Z
  (-) x Z           = x
  (-) (S x1) (S x2) = x1 - x2

  fromInteger :: Integer -> Nat
  fromInteger = toEnum . fromInteger

  negate :: Nat -> Nat
  negate = error "No implementation was required!"

  abs :: Nat -> Nat
  abs = error "No implementation was required!"

  signum :: Nat -> Nat
  signum = error "No implementation was required!"

instance Eq Nat where
  (==) :: Nat -> Nat -> Bool
  (==) Z Z           = True
  (==) (S x1) (S x2) = x1 == x2
  (==) _  _          = False

instance Ord Nat where
  compare Z Z         = EQ
  compare Z _         = LT
  compare _ Z         = GT
  compare (S x) (S y) = compare x y

isEven :: Nat -> Bool
isEven Z     = True
isEven (S x) = not $ isEven x

instance Real Nat where
  toRational :: Nat -> Rational
  toRational = toRational . fromEnum

instance Integral Nat where
  toInteger :: Nat -> Integer
  toInteger = toInteger . fromEnum

  div :: Nat -> Nat -> Nat
  div _ Z = error "Actual: division by zero!"
  div x1 x2
    | x1 < x2   = Z
    | otherwise = S $ div (x1 - x2) x2

  mod :: Nat -> Nat -> Nat
  mod _ Z = error "Actual: division by zero!"
  mod x1 x2
    | x1 < x2   = x1
    | otherwise = mod (x1 - x2) x2

  quotRem :: Nat -> Nat -> (Nat, Nat)
  quotRem = error "No implementation was required!"
