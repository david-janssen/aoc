module Day5

where


import Prelude

import Data.Foldable (maximum)
import RIO.List (sort)
import RIO.List.Partial (head)

import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- | Define the data representations

-- | Binary space partitioning
type BinSpace = [Bool]

-- | A seat consisting of a row and column 'BinSpace'
type Seat = (BinSpace, BinSpace)

-- | Read `B` as second half and anything else as first half
readRow :: Text -> BinSpace
readRow = map ('B' ==) . T.unpack

-- | Read `R` as second half and anything else as first half
readCol :: Text -> BinSpace
readCol = map ('R' ==) . T.unpack

-- | Read a 7 digit row and a 3 digit column
readSeat :: Text -> Seat
readSeat t = (readRow $ T.take 7 t, readCol $ T.drop 7 t)

-- | Load all the seats
loadSeats :: IO [Seat]
loadSeats = map readSeat . T.lines <$> loadDat "day5_seats.txt"


--------------------------------------------------------------------------------
-- | Solve the problem

-- | Convert BinSpace to Int
bsInt :: BinSpace -> Int
bsInt bs = sum $ [ 2^i | (i, b) <- zip [(0 :: Int)..] (reverse bs), b]

-- | Calculate seat ID
seatID :: Seat -> Int
seatID (r, c) = (bsInt r) * 8 + bsInt c

-- | Solve the puzzle
solve :: IO Int
solve = maximum . map seatID <$> loadSeats

-- >>> solve
-- 904


--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Solve the puzzle
solve2 :: IO Int
solve2 = do
  ids <- sort . map seatID <$> loadSeats
  let x = filter (\(a, b) -> a + 1 /= b) $ zip ids (drop 1 $ ids)
  pure $ (fst . head $ x) + 1

-- >>> solve2
-- 669
