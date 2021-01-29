{-# LANGUAGE InstanceSigs #-}

module Block1.Task3
  ( -- * Type
    NonEmpty(..)
  ) where

-- | Data type for non empty list
data NonEmpty a = a :| [a] deriving (Show)

-- | Implementation of Functor for NonEmpty
instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

-- | Implementation of Applicative for NonEmpty
instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure a = a :| []
  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  (<*>) (x1 :| x1s) (x2 :| x2s) = x1 x2 :| (fmap x1 x2s ++ (x1s <*> (x2 : x2s)))

-- | Implementation of Monad for NonEmpty
instance Monad NonEmpty where
  return :: a -> NonEmpty a
  return = pure
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  (x1 :| x1s) >>= f = x2 :| (x2s ++ xs)
    where toList :: NonEmpty a -> [a]
          toList (x :| x3s) = x : x3s
          x2 :| x2s = f x1
          xs = x1s >>= toList . f

-- | Implementation of Foldable for NonEmpty
instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x (foldr f z xs)

-- | Implementation of Traversable for NonEmpty
instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f = sequenceA . fmap f
