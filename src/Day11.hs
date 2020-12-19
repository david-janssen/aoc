module Day11

where

import Prelude hiding (Empty, lookup, _Empty)

import Coor

import RIO.List.Partial (head, tail)
import RIO.Partial (fromJust)

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- | Storing data in a grid

-- | The values an element of the grid can take
data Seat = Floor | Empty | Full
  deriving (Eq, Show)
makePrisms ''Seat

-- | Size of a grid in (w, h)
type Size = (Int, Int)

-- | A mapping from coordinates to seats
data Grid = Grid
  { _dim :: Size                -- ^ The width and height of the grid
  , _dat :: M.HashMap Coor Seat -- ^ The contents of the grid
  } deriving (Eq, Show)
makeLenses ''Grid

-- | Return the L-to-R, T-to-B walk over some grid-space
walk :: Size -> [[Coor]]
walk (w, h) = [[(x, y) | x <- [0 .. w - 1]] | y <- [0 .. h - 1]]

-- -- | Create a new grid by walkind some function of the coordinate and the old
-- -- grid over each coordinate.
-- walkWith :: (Grid -> Coor -> Seat) -> Grid -> Grid
-- walkWith f = \g -> Grid (d^.dim)

-- | Parse a single 'Seat'
cellP :: Parser Seat
cellP = Empty <$ char 'L' <|> Full <$ char '#' <|> Floor <$ char '.'

-- | Parse a 'Grid' of 'Seat's
gridP :: Parser Grid
gridP = do
  xs <- some (someTill cellP (void eol <|> eof))
  let wh = ((length $ head xs, length xs))
  pure . Grid wh . M.fromList . zip (concat $ walk wh) $ (concat xs)

-- | Lookup the coordinate in a grid
grab :: Grid -> Coor -> Maybe Seat
grab g c = g^.dat.at c

-- | Pretty-print a grid back into its textual representation
ppGrid :: Grid -> IO ()
ppGrid g = do
  let f Floor = '.'
      f Full  = '#'
      f Empty = 'L'
  putStrLn . unlines . map (map (f . fromJust . grab g)) . walk $ g^.dim

-- | Load the provided 'Grid' from disc
loadGrid :: IO Grid
loadGrid = parseDat "day11_seats.txt" gridP

-- | Load the smaller-scale example
loadTest :: IO Grid
loadTest = parseDat "day11_test.txt" gridP

--------------------------------------------------------------------------------
-- | Cutting paths across a grid in 8 directions

-- | Repeat an action until its result stabilizes
stabilize :: Eq a => (a -> a) -> a -> a
stabilize f a = let new = f a in if new == a then a else stabilize f new


-- | Return wether a coordinate falls in [0, w), [0, h)
bounded :: Coor -> Size -> Bool
bounded (x, y) (w, h) = x >= 0 && y >= 0 && x < w && y < h

--------------------------------------------------------------------------------
-- | Solve the first problem

-- | Evaluate how `busy` a seat is by counting its direct, occupied neighbors
busy1 :: Grid -> Coor -> Int
busy1 g c =
  let cs = c ^.. neighbors . filtered (`bounded` (g^.dim))
  in length $ cs ^.. folded . to (grab g) . _Just . _Full

-- | Step the grid once according to the rules
step1 :: Grid -> Grid
step1 g = Grid (g^.dim) $ M.mapWithKey f (g^.dat)
  where
    f _ Floor = Floor
    f c s = let n = busy1 g c in case s of
      Empty | n == 0 -> Full
      Full  | n > 3  -> Empty
      _              -> s

-- | Solve the first problem
solve1 :: IO Int
solve1 = count (Full ==) . view dat . stabilize step1 <$> loadGrid

-- >>> solve1
-- 2321

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Return the first non-floor seat visible in a direction from a coor
look :: Grid -> Coor -> Direction -> Maybe Seat
look g c d = let c' = shift 1 d c in case grab g c' of
  Just Floor -> look g c' d
  x          -> x

-- | Evaluate how `busy` a seat is by looking in all 8 directions
busy2 :: Grid -> Coor -> Int
busy2 g c = length $ [U .. UL] ^.. folded . to (look g c) . _Just . _Full

-- | Step the grid once according to the rules
step2 :: Grid -> Grid
step2 g = Grid (g^.dim) $ M.mapWithKey f (g^.dat)
  where
    f _ Floor = Floor
    f c s = let n = busy2 g c in case s of
      Empty | n == 0 -> Full
      Full  | n > 4  -> Empty
      _              -> s

-- | Solve the first problem
solve2 :: IO Int
solve2 = count (Full ==) . view dat . stabilize step2 <$> loadGrid

-- >>> solve2
-- 2102
