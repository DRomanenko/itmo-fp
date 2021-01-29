module Block2.Task2
  (
    -- * Functions
    splitOn
  ) where

import Data.List.NonEmpty as N (NonEmpty (..), head, tail, toList)

splitOn :: Char -> String -> NonEmpty String
splitOn delimiter = foldr (\symbol list ->
  if delimiter == symbol
  then [] :| toList list
  else (symbol : N.head list) :| N.tail list
  ) ([] :| [])
