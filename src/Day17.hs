module Day17

where

import Prelude hiding (Field2)

import RIO.Set.Partial (findMin, findMax)

import qualified RIO.HashMap as M
import qualified RIO.Set     as S
import qualified RIO.Text    as T

--------------------------------------------------------------------------------
-- | Solve the first problem

-- | Coordinates in 3d space
type XYZ = (Int, Int, Int)

-- | The collection of elements that are 'on'
type Field = S.Set XYZ

-- | Input for this problem
dat :: Field
dat = readField [ "#.##.##.", ".##..#.." , "....#..#" , ".##....#"
                , "#..##...", ".###..#." , "..#.#..#" , ".....#.." ]

-- | Example input for this problem
exl :: Field
exl = readField [".#.", "..#", "###"]

-- | Encode the provided input into a field
readField :: [Text] -> Field
readField ls = S.fromList . concat $
  [[(x, y, 0) | (x, c) <- zip [0..] (T.unpack l), c == '#']
              | (y, l) <- zip [0..] ls]

-- | A list of all 26 neighbors of a cube
neighbors :: XYZ -> [XYZ]
neighbors (x, y, z) = [(x + dx, y + dy, z + dz) | dx <- [-1, 0, 1]
                                                , dy <- [-1, 0, 1]
                                                , dz <- [-1, 0, 1]
                                                , any (/= 0) [dx, dy, dz]]

-- | Turn a finite list of items into counts of its occurences
counts :: (Ord a, Hashable a) => [a] -> M.HashMap a Int
counts = foldl' go M.empty
  where go acc x = M.insertWith (+) x 1 acc

-- | Step a field 1 cycle
step :: Field -> Field
step active = S.filter p $ (S.fromList $ M.keys neighs) <> active
  where
    neighs = counts . concatMap neighbors $ active
    look c = M.lookupDefault 0 c neighs
    p c    = let n = look c in
      if c `S.member` active then n `elem` [2, 3] else n == 3

-- | Solve the problem
solve :: Int
solve = S.size . step . step . step . step . step . step $ dat

-- >>> solve
-- 273

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Coordinates in 3d space
type XYZW = (Int, Int, Int, Int)
type Field2 = S.Set XYZW

-- | Input for this problem
dat2 :: Field2
dat2 = readField2 [ "#.##.##.", ".##..#.." , "....#..#" , ".##....#"
                , "#..##...", ".###..#." , "..#.#..#" , ".....#.." ]

-- | Example input for this problem
exl2 :: Field2
exl2 = readField2 [".#.", "..#", "###"]

-- | Encode the provided input into a field
readField2 :: [Text] -> Field2
readField2 ls = S.fromList . concat $
  [[(x, y, 0, 0) | (x, c) <- zip [0..] (T.unpack l), c == '#']
                 | (y, l) <- zip [0..] ls]

-- | A list of all neighbors of a cube
neighbors2 :: XYZW -> [XYZW]
neighbors2 (x, y, z, w) =
  [(x + dx, y + dy, z + dz, w + dw) | dx <- [-1, 0, 1]
                                    , dy <- [-1, 0, 1]
                                    , dz <- [-1, 0, 1]
                                    , dw <- [-1, 0, 1]
                                    , any (/= 0) [dx, dy, dz, dw]]


-- | Step a field 1 cycle
step2 :: Field2 -> Field2
step2 active = S.filter p $ (S.fromList $ M.keys neighs) <> active
  where
    neighs = counts . concatMap neighbors2 $ active
    look c = M.lookupDefault 0 c neighs
    p c    = let n = look c in
      if c `S.member` active then n `elem` [2, 3] else n == 3

solve2 :: Int
solve2 = S.size . step2 . step2 . step2 . step2 . step2 . step2 $ dat2

-- >>> solve2
-- 1504
