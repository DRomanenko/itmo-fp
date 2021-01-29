{-# LANGUAGE BangPatterns #-}

module Task1
    ( Point(..)
    , plus
    , minus
    , scalarProduct
    , crossProduct
    , perimeter
    , doubleArea
    , perimeterSlow
    , doubleAreaSlow
    ) where

-- | The data type of a point in two-dimensional space.
data Point =
  Point
    { x :: Int
    , y :: Int
    }
  deriving(Show, Eq)

-- | Implementation of the two point addition operator.
plus :: Point -> Point -> Point
(Point x1 y1) `plus` (Point x2 y2) = Point (x1 + x2) (y1 + y2)

-- | Implementation of the difference operation for two points.
minus :: Point -> Point -> Point
(Point x1 y1) `minus` (Point x2 y2) = Point (x1 - x2) (y1 - y2)

-- | Implementation of the scalar product of two points operation.
scalarProduct :: Point -> Point -> Int
(Point x1 y1) `scalarProduct` (Point x2 y2) = x1 * x2 + y1 * y2

-- | Implementation of a pseudoscalar product operation
-- ((x1, y1) * (x2, y2) = x1 * y2 - x2 * y1) of two points.
crossProduct  :: Point -> Point -> Int
(Point x1 y1) `crossProduct` (Point x2 y2) = x1 * y2 - x2 * y1

-- | Implementation of the function for fast calculating the perimeter of a polygon.
perimeter :: [Point] -> Double
perimeter []                = 0
perimeter points@(point:_)  = mySum (dist point) 0 points
  where
    dist :: Point -> Point -> Double
    dist (Point x1 y1) (Point x2 y2) =
      sqrt . fromIntegral $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    mySum :: (Point -> Double) -> Double -> [Point] -> Double
    mySum f !acc (point1:points'@(point2:_)) = mySum f (acc + dist point1 point2) points'
    mySum f !acc [ps]                        = acc + f ps
    mySum _ !acc _                           = acc

-- | Implementation of the function for fast calculating doubled area of a polygon.
doubleArea :: [Point] -> Int
doubleArea []               = 0
doubleArea points@(point:_) = abs $ mySum points 0
  where
    mySum :: [Point] -> Int -> Int
    mySum []                          _    = 0
    mySum [ps]                        !acc = crossProduct ps point + acc
    mySum (point1:points'@(point2:_)) !acc = mySum points' (crossProduct point1 point2 + acc)

-- | Implementation of the function for slow calculating the perimeter of a polygon.
perimeterSlow :: [Point] -> Double
perimeterSlow []      = 0
perimeterSlow points  = mySum 0 points + dist (last points) (head points)
  where
    dist :: Point -> Point -> Double
    dist (Point x1 y1) (Point x2 y2) =
      sqrt . fromIntegral $ (x2 - x1) * (x2 - x1) + (y2 - y1) * (y2 - y1)
    mySum :: Double -> [Point] -> Double
    mySum acc (point1:point2:ps) = mySum (acc + dist point1 point2) (point2:ps)
    mySum acc _                  = acc

-- | Implementation of the function for slow calculating doubled area of a polygon.
doubleAreaSlow :: [Point] -> Int
doubleAreaSlow []     = 0
doubleAreaSlow points = mySum (\point1 point2 -> crossProduct point1 point2) 0 points + se1 - se2
  where
    last'  = last points
    first' = head points
    se1    = x last' * y first'
    se2    = x first' * y last'
    mySum :: (Point -> Point -> Int) -> Int -> [Point] -> Int
    mySum f acc (point1:point2:ps) = mySum f (acc + f point1 point2) (point2:ps)
    mySum _ acc _                  = acc
