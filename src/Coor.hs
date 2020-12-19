-- | Some utilities for working with points in 2D space
module Coor

where

import Prelude


import RIO.List    (iterate)
import RIO.Partial (toEnum)

-- | 2D coordinates into the grid (0 indexed)
type Coor = (Int, Int)

-- | The 8 directions to consider
data Direction = U | UR | R | DR | D | DL | L | UL deriving (Eq, Show, Enum)

-- | Shift a coordinate step in a direction
shift :: Int -> Direction -> Coor -> Coor
shift n U  = over _2 (+n)
shift n R  = over _1 (+n)
shift n D  = over _2 (+ (-n))
shift n L  = over _1 (+ (-n))
shift n UR = shift n U . shift n R
shift n DR = shift n D . shift n R
shift n DL = shift n D . shift n L
shift n UL = shift n U . shift n L

-- | Return the ray from some point in some direction
ray :: Direction -> Coor -> [Coor]
ray = iterate . shift 1

-- | Fold from a coordinate to all its neighbors
neighbors :: Fold Coor Coor
neighbors = folding $ \c -> map (\d -> shift 1 d c) [U .. UL]

-- | Rotate a direction clockwise in steps of 8
rotate :: Int -> Direction -> Direction
rotate n = toEnum . (`mod` 8) . (+ n) . fromEnum

-- | Multiply both sides of a coordinate by some factor
mult :: Int -> Coor -> Coor
mult f (a, b) = (f * a, f * b)

-- | Add two coordinates
add :: Coor -> Coor -> Coor
add (a, b) (c, d) = (a + c, b + d)

-- | Pivot a coordinate around the origin in 4 steps
pivot :: Int -> Coor -> Coor
pivot n (a, b) = case n `mod` 4 of
  0 -> (a, b)
  1 -> (b, -a)
  2 -> (-a, -b)
  3 -> (-b, a)
  _ -> undefined
