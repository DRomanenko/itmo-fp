module Block1.Task1
  ( -- * Functions
    stringSum
  ) where

import Text.Read

-- | Safely sums up the numbers in a line.
stringSum :: String -> Maybe Int
stringSum line = fmap sum (traverse readMaybe (words line))
