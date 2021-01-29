{-# LANGUAGE InstanceSigs #-}

module Block3.Task1 where

import Control.Applicative (Alternative, empty, (<|>))
import Control.Monad ((>=>))
import Data.Bifunctor (first)

-- | Simple p-combinator
newtype Parser s a = Parser {runParser :: [s] -> Maybe (a, [s])}

-- | Implementation of Functor for Parser
instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser p) = Parser (fmap (first f) . p)

-- | Implementation of Applicative for Parser
instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)
  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  (<*>) (Parser p1) (Parser p2) = Parser (p1 >=> (\t -> (p2 . snd) t >>= \(a, r) -> Just (fst t a, r)))

-- | Implementation of Monad for Parser
instance Monad (Parser s) where
  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  (>>=) (Parser p) f = Parser (p >=> uncurry (runParser . f))

-- | Implementation of Alternative for Parser
instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser (const Nothing)
  (<|>) :: Parser s a -> Parser s a -> Parser s a
  (<|>) (Parser p1) (Parser p2) = Parser $ \s -> p1 s <|> p2 s
