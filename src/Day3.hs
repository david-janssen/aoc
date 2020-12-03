module Day3

where

import RIO

import qualified RIO.Set as S

--------------------------------------------------------------------------------
-- Define the representation and interface of the terrain

type Coor  = (Int, Int)
type Trees = S.Set Coor

-- | The width and height of the input pattern (which repeats)
width, height :: Int
width  = 11
height = 11

-- | A collection of coordinates that contain trees
trees :: Trees
trees = S.fromList $ [ (r, c) | (r, l) <- zip [0..] raw,
                                (c, x) <- zip [0..] l,
                                x == '#' ]
  where raw = [ "..##......."
              , "#...#...#.."
              , ".#....#..#."
              , "..#.#...#.#"
              , ".#...##..#."
              , "..#.##....."
              , ".#.#.#....#"
              , ".#........#"
              , "#.##...#..."
              , "#...##....#"
              , ".#..#...#.#"
              ]

-- | Return wether the provided Coor contains a tree
hasTree :: Trees -> Coor -> Bool
hasTree ts (r, c) = (r, c `mod` width) `elem` ts


--------------------------------------------------------------------------------
-- Define the walk and solve the problem

type Step = Int
type Walk = [Coor]

-- | Given a stepsize, return a list of all the coordinates we touch
genWalk :: Step -> Walk
genWalk w = map (\r -> (r, w * r)) [0..height]

-- | Given a Walk and some Trees, calculate the collisions
countBumps :: Trees -> Walk -> Int
countBumps ts = foldl' (\acc c -> if hasTree ts c then acc + 1 else acc) 0

-- | Put it all together
solve :: Step -> Int
solve = countBumps trees . genWalk

-- >>> solve 3
-- 7
