module Day11

where

import Prelude hiding (Empty, lookup)

-- import RIO.List.Partial (head)
-- import qualified RIO.Vector.Boxed as V

import qualified Grid as G

--------------------------------------------------------------------------------
-- | Define the data representation

-- | The values an element of the grid can take
data Seat = Floor | Empty | Full
  deriving (Eq, Show)


-- | The 8 directions to consider
data Direction = U | UR | R | DR | D | DL | L | UL deriving (Eq, Show, Enum)



-- | Parse a single 'Seat'
cellP :: Parser Seat
cellP = Empty <$ char 'L' <|> Full <$ char '#' <|> Floor <$ char '.'

-- | Parse a 'Grid' of 'Seat's
gridP :: Parser Grid
gridP = do
  xs <- some (someTill cellP (void eol <|> eof))
  pure $ Grid (V.fromList . concat $ xs) (Coor (length $ head xs, length xs))

--------------------------------------------------------------------------------
-- $ops

-- | Try to lookup a value in a grid
lookup :: Coor -> Grid -> Maybe Seat
lookup c@(Coor (x, y)) g | c < g^.dim = undefined

-- -- | Pretty-print the grid
-- pp :: Grid -> IO ()
-- pp g = mapM_ print . G.toList $ f <$> g
--   where f Floor = '.'
--         f Full  = '#'
--         f Empty = 'L'

-- -- | Load the provided 'Grid' from disc
-- loadGrid :: IO Grid
-- loadGrid = parseDat "day11_seats.txt" gridP

-- -- | Load the example to test stuff first
-- loadTest :: IO Grid
-- loadTest = parseDat "day11_test.txt" gridP

-- -- | Repeat an action until its result stabilizes
-- stabilize :: Eq a => (a -> a) -> a -> a
-- stabilize f a = let new = f a in if new == a then a else stabilize f new

-- --------------------------------------------------------------------------------
-- -- | Solve the second problem
-- --
-- -- The previous simulation was already a bit slow, we're going to take a
-- -- slightly different approach than the cell-wise algorithm from before.
-- -- Instead, per step we build up 8 grids (1 for each direction), then we combine
-- -- them into the next step.


-- -- | The coordinate-change matched to its direction
-- dStep :: Direction -> (Int, Int)
-- dStep U  = ( 0 ,  1)
-- dStep UR = ( 1 ,  1)
-- dStep R  = ( 1 ,  0)
-- dStep DR = (-1 ,  1)
-- dStep D  = ( 0 , -1)
-- dStep DL = (-1 , -1)
-- dStep L  = (-1 ,  0)
-- dStep UL = (-1 ,  1)

-- -- -- | Take all the slices through a grid for a direction.
-- -- slice :: Grid -> Direction -> [[Seat]]
-- -- slice g d = undefined

-- --   where
-- --     (w, h)   = size g
-- --     (dx, dy) = dStep d
-- --     ox =

-- --     ori =
