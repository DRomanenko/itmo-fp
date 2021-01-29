{-# LANGUAGE LambdaCase #-}

module Block2.Task2
  ( -- * Functions
    moving
  ) where

import Control.Monad.State
import Data.List

-- | Simple Moving Average Implementation
calculator :: (Real a, Fractional b) => Int -> [a] -> State [a] [b]
calculator n = \case
  [] -> return []
  (x : xs) -> do
    myState <- get
    let val = take n (x : myState)
    return ((if null val then 0 else realToFrac (sum val) / genericLength val) : evalState (calculator n xs) val)

-- | Calculates the unweighted mean of the previous n data
moving :: (Real a, Fractional b) => Int -> [a] -> [b]
moving n l = evalState (calculator n l) []
