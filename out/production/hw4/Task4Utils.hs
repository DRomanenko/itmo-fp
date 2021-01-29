{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

module Task4Utils
  ( HalyavaScriptVar(..)
  , ConvertToHalyavaScript(..)
  ) where

data Number
  = MyInt32 Int
  | MyDouble Double

instance Num Number where
  MyInt32 v1   + MyInt32 v2   = MyInt32  $ v1              + v2
  MyDouble v1  + MyInt32 v2   = MyDouble $ v1              + fromIntegral v2
  MyInt32 v1   + MyDouble v2  = MyDouble $ fromIntegral v1 + v2
  MyDouble v1  + MyDouble v2  = MyDouble $ v1              + v2

  MyInt32 v1   * MyInt32 v2   = MyInt32  $ v1              * v2
  MyDouble v1  * MyInt32 v2   = MyDouble $ v1              * fromIntegral v2
  MyInt32 v1   * MyDouble v2  = MyDouble $ fromIntegral v1 * v2
  MyDouble v1  * MyDouble v2  = MyDouble $ v1              * v2

  MyInt32 v1   - MyInt32 v2   = MyInt32  $ v1              - v2
  MyDouble v1  - MyInt32 v2   = MyDouble $ v1              - fromIntegral v2
  MyInt32 v1   - MyDouble v2  = MyDouble $ fromIntegral v1 - v2
  MyDouble v1  - MyDouble v2  = MyDouble $ v1              - v2

  abs = \case
    MyInt32 v  -> MyInt32  $ abs v
    MyDouble v -> MyDouble $ abs v

  signum = \case
    MyInt32 v  -> MyInt32  $ signum v
    MyDouble v -> MyDouble $ signum v

  fromInteger v = MyInt32 $ fromIntegral v

instance Show Number where
  show = \case
    MyInt32 v  -> show v
    MyDouble v -> show v

instance Eq Number where
  MyInt32 v1   == MyInt32 v2  = v1               == v2
  MyDouble v1  == MyInt32 v2  = v1               == fromIntegral v2
  MyInt32 v1   == MyDouble v2 = fromIntegral v1  == v2
  MyDouble v1  == MyDouble v2 = v1               == v2

instance Ord Number where
  MyInt32 v1   `compare` MyInt32 v2   = v1               `compare` v2
  MyDouble v1  `compare` MyInt32 v2   = v1               `compare` fromIntegral v2
  MyInt32 v1   `compare` MyDouble v2  = fromIntegral v1  `compare` v2
  MyDouble v1  `compare` MyDouble v2  = v1               `compare` v2


data HalyavaScriptVar
  = Num Number
  | Str String
  | Boolean Bool
  | NaN

instance Num HalyavaScriptVar where
  Num v1     + Num v2      = Num $ v1                       + v2
  Str v1     + Str v2      = Str $ v1                       <> v2
  Str v1     + Num v2      = Str $ v1                       <> show v2
  Num v1     + Str v2      = Str $ show v1                  <> v2
  Boolean v1 + Boolean v2  = Boolean $ v1                   || v2
  Boolean v1 + Num v2      = Num $ (MyInt32 $ fromEnum v1)  + v2
  Boolean v1 + Str v2      = Str $ (show v1)                <> v2
  NaN        + _           = NaN
  v1         + v2          = v2                             + v1

  Num v1      * Num v2      = Num $ v1                      * v2
  Str _       * Str _       = NaN
  Str _       * Num _       = NaN
  Num _       * Str _       = NaN
  Boolean v1  * Boolean v2  = Boolean $ v1                  && v2
  Boolean v1  * Num v2      = Num $ (MyInt32 $ fromEnum v1) * v2
  Boolean _   * Str _       = NaN
  NaN         * _           = NaN
  v1          * v2          = v2                            * v1

  Num v1      - Num v2      = Num $ v1                      - v2
  Str _       - Str _       = NaN
  Str _       - Num _       = NaN
  Num _       - Str _       = NaN
  Boolean v1  - Boolean v2  = Num $ MyInt32 $ fromEnum v1   - fromEnum v2
  Boolean v1  - Num v2      = Num $ (MyInt32 $ fromEnum v1) - v2
  Boolean _   - Str _       = NaN
  NaN         - _           = NaN
  v1          - v2          = v2                            - v1

  abs = \case
    Num v -> Num $ abs v
    v     -> v

  signum = \case
    Num v     -> Num $ signum v
    Boolean _ -> Boolean $ True
    _         -> NaN

  fromInteger v = Num $ MyInt32 $ fromIntegral v

instance Show HalyavaScriptVar where
  show = \case
    Num v     -> show v
    Str v     -> show v
    Boolean v -> show v
    NaN       -> "NaN"

instance Eq HalyavaScriptVar where
  Num v1     == Num v2      = v1                     == v2
  Str v1     == Str v2      = v1                     == v2
  Str v1     == Num v2      = v1                     == show v2
  Boolean v1 == Boolean v2  = v1                     == v2
  Boolean v1 == Num v2      = MyInt32 (fromEnum v1)  == v2
  Boolean v1 == Str  v2     = show v1                == v2
  NaN        == _           = True
  v1         == v2          = v2                     == v1

instance Ord HalyavaScriptVar where
  Num v1     `compare` Num v2      = v1                     `compare` v2
  Str v1     `compare` Str v2      = v1                     `compare` v2
  Str v1     `compare` Num v2      = v1                     `compare` show v2
  Boolean v1 `compare` Boolean v2  = v1                     `compare` v2
  Boolean v1 `compare` Num v2      = MyInt32 (fromEnum v1)  `compare` v2
  Boolean v1 `compare` Str  v2     = show v1                `compare` v2
  NaN        `compare` _           = LT
  v1         `compare` v2          = v2                     `compare` v1

-- | Class to convert value to 'HS' representation.
class Show v => ConvertToHalyavaScript v where
  convertToHalyavaScript :: v -> HalyavaScriptVar

instance ConvertToHalyavaScript Bool where
  convertToHalyavaScript = Boolean

instance ConvertToHalyavaScript Int where
  convertToHalyavaScript = Num . MyInt32

instance ConvertToHalyavaScript Double where
  convertToHalyavaScript = Num . MyDouble

instance ConvertToHalyavaScript String where
  convertToHalyavaScript = Str
