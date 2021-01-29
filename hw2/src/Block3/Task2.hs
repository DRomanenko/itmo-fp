{-# LANGUAGE LambdaCase #-}
module Block3.Task2
  ( -- * Functions
    element
  , eof
  , ok
  , satisfy
  , stream
  ) where

import Block3.Task1

-- | The parser never falls down or absorbs the injectile
ok :: Monoid a => Parser s a
ok = Parser $ \s -> Just (mempty, s)

-- | Checks that the parser has reached
-- the end of the data stream (otherwise it drops)
eof :: Monoid s => Parser s ()
eof = Parser $ \case
  [] -> Just ((), [])
  _  -> Nothing

-- | The parser takes a predicate on an item in
-- the thread, and returns the item by absorbing it from
-- the thread if the predicate on the item is True, otherwise it drops
satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \case
  []       -> Nothing
  (x : xs) -> if p x then Just (x, xs) else Nothing

-- | Parse one or more elements of the flow like char
element :: Eq s => s -> Parser s s
element = satisfy . (==)

-- | Parse one or more elements of the flow string
stream :: Eq s => [s] -> Parser s [s]
stream = traverse element
