module Day1

where

import Prelude

import RIO.List (find)
import RIO.Partial (read, fromJust)
import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- | Solve the puzzle

-- | Load all the numbers from file
vals :: IO [Int]
vals = map (read . T.unpack :: Text -> Int) . T.lines <$> loadDat "day1_nums.txt"

-- | Find the first pair in a list of items that satisfies some predicate
pairFind :: (a -> a -> Bool) -> [a] -> Maybe (a, a)
pairFind p (a:xs) = case find (p a) xs of
  Just b  -> Just (a, b)
  Nothing -> pairFind p xs
pairFind _ _ = Nothing

-- | Solve the puzzle
solve :: IO Int
solve = uncurry (*) . fromJust . pairFind (\a b -> a + b == 2020) <$> vals

-- >>> solve
-- 902451

--------------------------------------------------------------------------------
-- | Solve the second puzzle

tripFind :: (a -> a -> a -> Bool) -> [a] -> Maybe (a, a, a)
tripFind p (a:xs) = case pairFind (p a) xs of
  Just (b, c) -> Just (a, b, c)
  Nothing -> tripFind p xs
tripFind _ _ = Nothing

solve2 :: IO Int
solve2 = do
  (a, b, c) <- fromJust . tripFind (\a b c -> a + b + c == 2020) <$> vals
  pure $ a * b * c

-- >>> solve2
-- 85555470
