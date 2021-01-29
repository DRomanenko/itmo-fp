{-# LANGUAGE InstanceSigs #-}

module Block1.Task2
  ( -- * Type
    Tree(..)
  ) where

-- | Data type for binary tree
data Tree a
  = Branch (Tree a) (Tree a)
  | Leaf a
  deriving (Show, Eq)

-- | Implementation of Functor for Tree
instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a)     = Leaf (f a)
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

-- | Implementation of Applicative for Tree
instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf
  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) (Leaf a)             = Leaf (f a)
  (<*>) (Branch l r) x@(Leaf _)       = Branch (l <*> x) (r <*> x)
  (<*>) x@(Leaf _) (Branch l r)       = Branch (x <*> l) (x <*> r)
  (<*>) (Branch l1 r1) (Branch l2 r2) = Branch (l1 <*> l2) (r1 <*> r2)

-- | Implementation of Foldable for Tree
instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf x)     = f x z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

-- | Implementation of Traversable for Tree
instance Traversable Tree where
  traverse :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a)     = Leaf <$> f a
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r
