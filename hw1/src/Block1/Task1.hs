{-# LANGUAGE LambdaCase #-}

module Block1.Task1
  ( -- * Type
    WeekDays(..)

    -- * Functions
  , afterDays
  , daysToParty
  , isWeekend
  , nextDay
  ) where

-- | Data type for the week days.
data WeekDays
  = Monday    -- ^ monday
  | Tuesday   -- ^ tuesday
  | Wednesday -- ^ wednesday
  | Thursday  -- ^ thursday
  | Friday    -- ^ friday
  | Saturday  -- ^ saturday
  | Sunday    -- ^ sunday
  deriving (Show)

-- | Turns WeekDays into a number and number in WeekDays.
instance Enum WeekDays where
  fromEnum = \case
    Monday    -> 0
    Tuesday   -> 1
    Wednesday -> 2
    Thursday  -> 3
    Friday    -> 4
    Saturday  -> 5
    Sunday    -> 6

  toEnum = \case
    0 -> Monday
    1 -> Tuesday
    2 -> Wednesday
    3 -> Thursday
    4 -> Friday
    5 -> Saturday
    6 -> Sunday
    _ -> error "It's not the week day!"

-- | Returns the day of the week following
-- the transferred day.
nextDay :: WeekDays -> WeekDays
nextDay day = afterDays day 1

-- | Returns the day of the week that will
-- come after the specified number of days.
afterDays :: WeekDays -> Int -> WeekDays
afterDays day shift = toEnum $ (shift + fromEnum day) `mod` 7

-- | Checks if the day of the week is a day off.
isWeekend :: WeekDays -> Bool
isWeekend = \case
  Saturday -> True
  Sunday   -> True
  _        -> False

-- | Returns the number of days left until Friday.
daysToParty :: WeekDays -> Int
daysToParty day = (fromEnum Friday - fromEnum day) `mod` 7
