module Day9

where

import Prelude

import RIO.List (find)

import qualified RIO.Vector.Unboxed as V
import RIO.Vector.Unboxed.Partial ((!), tail, minimum, maximum)

--------------------------------------------------------------------------------
-- | Define data representation

-- | The signal is a series of integer numbers
type Signal = V.Vector Int

-- | Load the signal from disc
loadSignal :: IO Signal
loadSignal = V.fromList <$> (parseDat "day9_signal.txt" $ some (lexeme intP))


--------------------------------------------------------------------------------
-- | Define some data operations

-- | Find the first pair of values from a signal that sums to some number
findPSum :: Signal -> Int -> Maybe (Int, Int)
findPSum s n | V.length s < 2 = Nothing
             | otherwise      = case V.find (n - (s ! 0) ==) (tail s) of
                 Just m  -> Just (n, m)
                 Nothing -> findPSum (tail s) n

--------------------------------------------------------------------------------
-- | Solve the problem

-- | Find the first number that cannot be calculated as a sum of the 25 preceding numbers
solve :: IO Int
solve = do
  s <- loadSignal
  let p i = findPSum (V.slice i 25 s) (s ! (i + 25))
  case find (isNothing . p) [0..974] of
    Just i  -> pure $ s ! (i + 25)
    Nothing -> undefined

-- >>> solve
-- 1504371145

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Brute-force find the span in the signal that sums to the result from `solve`
--
-- NOTE: This could be much faster by looping more intelligently, but why?
solve2 :: IO Int
solve2 = do
  tgt <- solve
  s   <- loadSignal
  let spans = [(i, n) | i <- [0..1000]
                      , n <- [1..(1000 - i)]]
  let p (i, n) = sum (V.toList $ V.slice i n s) == tgt
  case find p spans of
    Just (i, n) -> do
      let s2 = V.slice i n s
      pure $ (minimum s2) + (maximum s2)
    Nothing     -> undefined

-- >>> solve2
-- 183278487
