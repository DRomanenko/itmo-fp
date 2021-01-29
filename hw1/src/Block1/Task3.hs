{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE LambdaCase   #-}

module Block1.Task3
  (
    -- * Type
    Tree(..)

    -- * Functions
  , erase
  , find
  , fromList
  , insert
  , isEmpty
  , size
  ) where

import Data.List.NonEmpty as N (NonEmpty (..), head, length, (<|))

data Tree a
  = Leaf
  | Node
    {
      value   :: N.NonEmpty a
      , left  :: Tree a
      , right :: Tree a
    }
  deriving (Show)

isEmpty :: Tree a -> Bool
isEmpty = \case
  Leaf -> True
  _    -> False

size :: Tree a -> Int
size = \case
  Leaf       -> 0
  Node v l r -> N.length v + size l + size r

find :: Ord a => Tree a -> a -> Maybe a
find tree x =
  case tree of
    Leaf       -> Nothing
    Node v l r ->
      case compare val x of
        EQ -> Just val
        GT -> find l x
        LT -> find r x
      where
        val = N.head v

insert :: Ord a => Tree a -> a -> Tree a
insert tree x =
  case tree of
    Leaf       -> Node (x :| []) Leaf Leaf
    Node v l r ->
      case compare (N.head v) x of
        EQ -> Node (x <| v) l r
        GT -> Node v (insert l x) r
        LT -> Node v l (insert r x)

fromList :: Ord a => [a] -> Tree a
fromList = foldl insert Leaf

erase :: Ord a => Tree a -> a -> Tree a
erase tree x =
  case tree of
    Leaf -> Leaf
    n@(Node v l r) ->
      case compare (N.head v) x of
        EQ -> helper n
        GT -> Node v (erase l x) r
        LT -> Node v l (erase r x)
      where
        helper :: Tree a -> Tree a
        helper = \case
          Leaf           -> Leaf
          Node _ Leaf r1 -> r1
          Node _ l1 Leaf -> l1
          Node _ l1 r1   -> Node (value l1) l1 (helper r1)

instance Foldable Tree where
  foldMap :: Monoid m => (a -> m) -> Tree a -> m
  foldMap _ Leaf            = mempty
  foldMap func (Node v l r) = foldMap func l <> foldMap func v <> foldMap func r

  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr _ e Leaf            = e
  foldr func e (Node v l r) = foldr func (foldr func (foldr func e r) v) l
