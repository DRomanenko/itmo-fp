{-# LANGUAGE LambdaCase   #-}
{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Task4
  ( HalyavaScript(..)

  , interpretOneArg
  , log2
  , interpretTwoArgs
  , mySum
  , convertToHalyavaScript
  ) where

import Control.Applicative (liftA2)
import Control.Monad.ST (ST, runST)
import Data.Kind (Type)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import Task4Utils (ConvertToHalyavaScript (..), HalyavaScriptVar (..))

infixl 1 #
infix 4 @=
infix 4 @>, @<, @>=, @<=, @==, @/=
infix 6 @+, @-
infix 7 @*

-- | A class that describes the syntax, HalyavaScript.
class HalyavaScript e where
  type Variable e v :: Type
  sVariable
    :: e (Variable e HalyavaScriptVar) -> e HalyavaScriptVar
  sWithVar
    :: ConvertToHalyavaScript a => a -> (e (Variable e HalyavaScriptVar) -> e ()) -> e ()
  (#)
    :: e a -> e b -> e b
  (@=)
    :: e (Variable e HalyavaScriptVar) -> e HalyavaScriptVar -> e ()
  (@>), (@<), (@>=), (@<=), (@==), (@/=)
    :: e HalyavaScriptVar -> e HalyavaScriptVar -> e Bool
  (@+), (@-), (@*)
    :: e HalyavaScriptVar -> e HalyavaScriptVar -> e HalyavaScriptVar
  sIf
    :: e Bool -> e () -> e () -> e ()
  sWhile
    :: e Bool -> e () -> e ()
  sFun1
    :: (e HalyavaScriptVar -> e (Variable e HalyavaScriptVar) -> e ())
      -> e HalyavaScriptVar
      -> e HalyavaScriptVar
  sFun2
    :: (e HalyavaScriptVar -> e HalyavaScriptVar -> e (Variable e HalyavaScriptVar) -> e ())
      -> e HalyavaScriptVar
      -> e HalyavaScriptVar
      -> e HalyavaScriptVar

-- | Implementation of HalyavaScript class operators and functions.
instance HalyavaScript (ST s) where
  type Variable (ST s) v = STRef s v

  var @= arg = do
    sVar <- var
    sArg <- arg
    writeSTRef sVar sArg

  sVariable sVar = do
    var <- sVar
    readSTRef var

  sWithVar var body = do
    sVar <- newSTRef $ convertToHalyavaScript var
    body (return sVar)

  (#)   = (>>)

  (@>)  = liftA2 (>)
  (@<)  = liftA2 (<)
  (@>=) = liftA2 (>=)
  (@<=) = liftA2 (<=)
  (@==) = liftA2 (==)
  (@/=) = liftA2 (/=)

  (@+)  = liftA2 (+)
  (@-)  = liftA2 (-)

  (@*)  = liftA2 (*)

  sIf condition body1 body2 = do
    isTrue <- condition
    if (isTrue) then body1 else body2

  sWhile condition body = do
    isTrue <- condition
    if (isTrue)
      then do
        _ <- body
        sWhile condition body
      else pure ()

  sFun1 body v = do
    res <- newSTRef NaN
    body v $ return res
    readSTRef res

  sFun2 body v1 v2 = do
    res <- newSTRef NaN
    body v1 v2 $ return res
    readSTRef res

-- | Implementation of a HalyavaScript interpreter function for functions from a single argument.
interpretOneArg :: ConvertToHalyavaScript e =>
 e -> (forall s . ST s HalyavaScriptVar -> ST s HalyavaScriptVar)
  -> HalyavaScriptVar
interpretOneArg v body = runST $ body $ return (convertToHalyavaScript v)

-- | Implementation of a function for a given @x@ calculates @ceiling (log2 (a))@.
log2 :: HalyavaScript e =>
  e HalyavaScriptVar -> e HalyavaScriptVar
log2 =
  sFun1 $ \v logCnt ->
    sWithVar (1 :: Int) $ \accum ->
    sWithVar (0 :: Int) $ \c0 ->
    sWithVar (1 :: Int) $ \c1 ->
      logCnt @= sVariable c0 #
      sWhile (v @> sVariable accum)
        ( accum @= sVariable accum @+ sVariable accum #
          logCnt @= sVariable logCnt @+ sVariable c1
        )

-- | Implementation of a HalyavaScript interpreter function for functions from two arguments.
interpretTwoArgs :: ConvertToHalyavaScript e =>
 e -> e -> (forall s . ST s HalyavaScriptVar -> ST s HalyavaScriptVar -> ST s HalyavaScriptVar)
  -> HalyavaScriptVar
interpretTwoArgs v1 v2 body =
  runST $ body (return $ convertToHalyavaScript v1) (return $ convertToHalyavaScript v2)

-- | Implementation of a function that counts the sum of two arguments.
mySum :: HalyavaScript e =>
  e HalyavaScriptVar -> e HalyavaScriptVar -> e HalyavaScriptVar
mySum =
  sFun2 $ \v1 v2 res ->
    res @= v1 @+ v2
