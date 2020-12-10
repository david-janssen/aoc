module Day10

where

import Prelude hiding (Pos)

import RIO.List (sort, scanl)
import RIO.List.Partial (tail, last, head)
import RIO.State

--------------------------------------------------------------------------------
-- | Define the data representation

-- | An 'Adapter' is purely defined by its joltage rating
type Adapter = Int

-- | Load the adapters from disc
--
-- Manually add the 0 and (max + 3) for the source and sink
loadAdapters :: IO [Adapter]
loadAdapters = do
  raw <- sort <$> (parseDat "day10_joltage.txt" $ some (lexeme intP))
  pure $ (0:) raw <> [(last raw + 3)]

-- | Calculate the stepsizes between entries
steps :: Num a => [a] -> [a]
steps as = map (uncurry . flip $ (-)) $ zip as (tail as)

--------------------------------------------------------------------------------
-- | Solve the problem

-- | Count the step-sizes
solve :: IO Int
solve = do
  as <- steps <$> loadAdapters
  let ones   = count (== 1) as
  let threes = count (== 3) as
  pure $ ones * threes

-- >>> solve
-- 1755

--------------------------------------------------------------------------------
-- | Solve the second problem
--
-- We solve the problem like this:
-- 1. We work on a reversed list of cummulative stepsizes
-- 2. For any entry in the list, its possible paths is equal to the sum of all
--    previous paths that fall within a 3 jump distance of the current value.
-- 3. We keep a working stack for calculations

type Pos   = Int          -- | Where we are in the calculation
type Val   = Int          -- | The possible number of paths at a given 'Pos'
type Stack = [(Pos, Val)] -- | A working (sorted) stack of intermediate values

-- | For a position, return the sum of all values that fall within distance 3
sumPaths :: Pos -> Stack -> Val
sumPaths p = sum . map snd . takeWhile (\y -> p - (fst y) <= 3)

-- | Do 1 step in the stateful computation
step :: Pos -> State Stack ()
step p = modify $ \ys -> let v = sumPaths p ys in (p, v):ys

-- | Run the computation
solve2 :: IO Int
solve2 = do
  as <- tail . scanl (+) 0 . reverse . steps <$> loadAdapters
  pure . snd . head . execState (mapM_ step as) $ [(0, 1)]

-- >>> solve2
-- 4049565169664
