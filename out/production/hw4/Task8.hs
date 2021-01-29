{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE InstanceSigs  #-}
{-# LANGUAGE LambdaCase    #-}

module Task8
  ( Grid(..)
  , State(..)
  , gridAfter
  , genGrid
  , gridSize
  , nextDay
  , showGrid
  ) where

import Control.Comonad
import Control.Monad (liftM2)
import Data.List
import System.Random

data ListZipper a =
  LZ [a] a [a]
  deriving Functor

listLeft, listRight :: ListZipper a -> ListZipper a
listLeft  (LZ (a:as) x bs) = LZ as a (x:bs)
listLeft _                 = error "listLeft"
listRight (LZ as x (b:bs)) = LZ (x:as) b bs
listRight _                = error "listRight"

listWrite :: a -> ListZipper a -> ListZipper a
listWrite x (LZ ls _ rs) = LZ ls x rs

toList :: ListZipper a -> Int -> [a]
toList (LZ ls x rs) n = reverse (take n ls) ++ [x] ++ take n rs

iterateTail :: (a -> a) -> a -> [a]
iterateTail f = tail . iterate f

genericMove :: (z a -> z a) -> (z a -> z a) -> z a -> ListZipper (z a)
genericMove f g e = LZ (iterateTail f e) e (iterateTail g e)

instance Comonad ListZipper where
  extract :: ListZipper a -> a
  extract (LZ _ x _) = x
  duplicate :: ListZipper a -> ListZipper (ListZipper a)
  duplicate = genericMove listLeft listRight


newtype Grid a =
  Grid
    { unGrid :: ListZipper (ListZipper a)
    }
  deriving Functor

up, down :: Grid a -> Grid a
up   (Grid g) = Grid (listLeft  g)
down (Grid g) = Grid (listRight g)

left, right :: Grid a -> Grid a
left  (Grid g) = Grid (fmap listLeft  g)
right (Grid g) = Grid (fmap listRight g)

gridRead :: Grid a -> a
gridRead (Grid g) = extract $ extract g

gridWrite :: a -> Grid a -> Grid a
gridWrite x (Grid g) = Grid $ listWrite newLine g
  where
    oldLine = extract g
    newLine = listWrite x oldLine

horizontal, vertical :: Grid a -> ListZipper (Grid a)
horizontal = genericMove left right
vertical   = genericMove up   down

instance Comonad Grid where
  extract :: Grid a -> a
  extract = gridRead
  duplicate :: Grid a -> Grid (Grid a)
  duplicate = Grid . fmap horizontal . vertical

neighbours :: [Grid a -> Grid a]
neighbours = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
  where horizontals = [left, right]
        verticals   = [up, down]

allNeighbours :: Grid State -> [State]
allNeighbours g = map (\direction -> extract $ direction g) neighbours

evolve :: Grid State -> Grid State
evolve = extend nextDay

-- | Implementation of a data type that stores the state of a person at the moment.
data State
  = Infected StdGen Int   -- ^ The person is actively infected.
  | Immunized StdGen Int  -- ^ The person has recovered and has an active immune system.
  | Sick StdGen Int       -- ^ The person is sick.
  | Healthy StdGen        -- ^ The person is healthy and has no immunity.

-- | Implementation of a function that outputs the Grid of diseases after 'n' days.
gridAfter :: Int -> Grid State -> Grid State
gridAfter n g = if n == 0 then g else gridAfter (n - 1) $ evolve g

-- | Probability of Infection
probabilityInfection :: Double
probabilityInfection = 0.4

-- | Duration of states.
durationInfected, durationImmunized, durationSick :: Int
durationInfected = 3
durationImmunized = 15
durationSick = 10

-- | Grid size (n * n)
gridSize :: Int
gridSize = 30

-- | Implementation of a function that generates a starting grid.
genGrid :: Grid State
genGrid =
  gridWrite (Infected (mkStdGen 0) durationInfected)
  $ fmap (\v -> Healthy (mkStdGen v))
  $ Grid $ duplicate $ LZ (iterate (+ (-1)) (-1)) 0 (iterate (+ 1) 1)

-- | Implementation of a function that determines the state of a person for the next day.
nextDay :: Grid State -> State
nextDay g =
  case (extract g, allNeighbours g) of
    (Infected f v, _)   -> if (v == 0) then (Sick f durationSick) else (Infected f (v - 1))
    (Immunized f v, _)  -> if (v == 0) then (Healthy f) else (Immunized f (v - 1))
    (Sick f v, _)       -> if (v == 0) then (Immunized f durationImmunized) else (Sick f (v - 1))
    (Healthy f, around) -> do
          let probabilityBorder = 1.0 -
                ((1.0 - probabilityInfection) :: Double) ^
                ((foldl' (+) 0 (map haveChance around)) :: Int)
          let (personProbability, newGen) = random f :: (Double, StdGen)
          if (personProbability <= probabilityBorder)
            then Infected newGen durationInfected
            else Healthy newGen
          where
            haveChance :: State -> Int
            haveChance = \case
              Infected _ _  -> 1
              Immunized _ _ -> 0
              Sick _ _      -> 1
              Healthy _     -> 0

-- | Implementation of a function that converts the grid to a String.
showGrid :: Grid State -> Int -> String
showGrid grid size = do
  let (Grid newGrid) = (fmap matchState grid)
  intercalate "\n" (map ((intercalate "") . (`toList` size)) (toList newGrid size))
  where
    matchState :: State -> String
    matchState = \case
      (Infected _ _)  -> "#"
      (Immunized _ _) -> "@"
      (Sick _ _)      -> "-"
      (Healthy _)     -> " "
