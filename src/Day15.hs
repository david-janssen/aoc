{-# LANGUAGE BangPatterns #-}
module Day15

where

import Prelude

import Control.Monad (replicateM)
import RIO.State

import Data.STRef

import qualified RIO.HashMap      as M
import qualified RIO.List.Partial as L (last, init)


-- | The starting sequence of numbers
input :: [Int]
input = [1, 2, 16, 19, 18, 0]

-- | Example sequence
ex1 :: [Int]
ex1 = [1, 3, 2]

--------------------------------------------------------------------------------
-- | Solve the first problem as a stateful computation


-- | The computational environment for the memory game.
--
-- NOTE, we store 'last', and 'turn', although that information already exists
-- in 'mem', but this saves us from having to look it up, saving cycles.
--
data Env = Env
  { _mem  :: !(M.HashMap Int Int) -- ^ Storage of on which turn a number last occured
  , _cur  :: !Int                 -- ^ The current number we are considering
  , _turn :: !Int                 -- ^ The turn number (1..)
  } deriving (Eq, Show)
makeLenses ''Env

-- | Create the environment for a sequence of starting numbers
initEnv :: [Int] -> Env
initEnv xs = Env (M.fromList (zip (L.init xs) [1..])) (L.last xs) (length xs)

-- | The default environment for my input signal
defEnv :: Env
defEnv = initEnv input

-- | Iterate the game 1 step, also returning the most recently produced number
step :: State Env Int
step = get >>= \e -> do

  -- Calculate the next value in the sequence
  let nxt = case M.lookup (e^.cur) (e^.mem) of
        Nothing -> 0
        Just l  -> (e^.turn) - l

  -- Update the Env
  mem  %= M.insert (e^.cur) (e^.turn)
  turn += 1
  cur  .= nxt
  pure nxt

-- | Get the Nth number in a sequence for some environment
getNth :: Int -> [Int] -> Int
getNth n xs = let e = initEnv xs in
  (execState (replicateM_ (n - e^.turn) step) e)^.cur

-- | Solve the problem
solve :: Int
solve = getNth 2020 input

-- >>> solve
-- 536

--------------------------------------------------------------------------------
-- Solve the 2nd problem
--
-- Using the 'State' method from above produces a stack-overflow because 'State'
-- is lazy... d'oh!

-- | Strictly recursive version of getNth, could probably use foldl' instead
--
-- NOTE: the strictness annotations are very much necessary
getNth2 :: Int -> [Int] -> Int
getNth2 n xs = let env = initEnv xs in go (n - env^.turn) env
  where
    go 0 e = e^.cur
    go !n !e = go (n - 1) (execState step e)

-- | Solve the second problem
solve2 :: Int
solve2 = getNth2 30000000 input

-- >>> solve2
-- 24065124
