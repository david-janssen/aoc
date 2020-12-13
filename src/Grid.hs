-- | Some utilities for working with 2D grids of things
module Grid

where

import Prelude

import Control.Arrow ((|||))

import RIO.List.Partial (head)
import qualified RIO.Vector.Boxed as V


--------------------------------------------------------------------------------
-- $coor
--
-- We define a 'Coor' type as a 2D int and provide some basic maths operations
-- and a lensy interface. Further down we define directions and how they
-- interact with coordinates to shift and create slices through space.

-- | Indices into a grid
newtype Coor = Coor { unCoor :: (Int, Int) } deriving (Eq, Show)
makeWrapped ''Coor

-- | Lenses into the two axes of a coordinate
_x, _y :: Lens' Coor Int
_x = _Wrapped . _1
_y = _Wrapped . _2

-- | Basic maths with coordinates
instance Num Coor where
  (Coor (a, b)) + (Coor (c, d)) = Coor (a + c, b + d)
  (Coor (a, b)) * (Coor (c, d)) = Coor (a * c, b * d)
  abs (Coor (a, b)) = Coor (abs a, abs b)
  signum (Coor (a, b)) = Coor (signum a, signum b)
  negate (Coor (a, b)) = Coor (negate a, negate b)
  -- | Bit silly, but this makes (Coor (a, b)) * x work correctly
  fromInteger i = Coor (fromInteger i, fromInteger i)

-- | How to embed and project things to and from coordinates
class IsCoor a where
  coor :: Prism' Coor a

--------------------------------------------------------------------------------
-- $grid
--
-- We define a 'Grid' type that stores things on a 2D (square) grid of
-- coordinates. Internally we store everything in a simple 1D vector, and we
-- translate coordinate-indices into the 1D vector index space.

-- | Specialize the type of Grid to seats
data Grid a = Grid
  { _dim :: Coor       -- ^ Non-inclusive max coor
  , _dat :: V.Vector a -- ^ We store data in a simple 1D vector
  } deriving (Eq, Show)
makeLenses ''Grid

-- | Make a grid from a nested list, error if input is invalid
fromList :: [[a]] -> Grid a
fromList [] = error "Cannot make an empty grid"
fromList as = let (w, h) =  (length as, length . head $ as) in
  if any ((w /=) . length) as
  then error "Input is not square"
  else Grid (Coor (w, h)) . V.fromList . concat $ as

-- | Lenses into size of Grid
_w, _h :: Lens' (Grid a) Int
_w = dim . _x
_h = dim . _y

-- | An instance for indexed to access data in the grid
type instance Index (Grid a)   = Coor
type instance IxValue (Grid a) = a

-- | How to index into a grid
instance Ixed (Grid a) where
  ix c h g = Grid (g^.dim) <$> (traverseOf (ix l) h (g^.dat))
    where l = (c^._y) * (g^._w) + (c^._x)

--------------------------------------------------------------------------------
-- $dirs

-- | The 8 grid-directions
data Direction = U | UR | R | DR | D | DL | L | UL  deriving (Eq, Show, Enum)

-- | The coordinates related to directions
instance IsCoor Direction where
  coor = prism' project embed
    where
      project U  = Coor (0, 1)
      project R  = Coor (1, 0)
      project D  = Coor (0, -1)
      project L  = Coor (-1, 0)
      project UR = Coor (1, 1)
      project DR = Coor (1, -1)
      project DL = Coor (-1, -1)
      project UL = Coor (-1, 1)

      embed (Coor (0, 1))   = Just U
      embed (Coor (1, 0))   = Just R
      embed (Coor (0, -1))  = Just D
      embed (Coor (-1, 0))  = Just L
      embed (Coor (1, 1))   = Just UR
      embed (Coor (1, -1))  = Just DR
      embed (Coor (-1, -1)) = Just DL
      embed (Coor (-1, 1))  = Just UL
      embed _ = Nothing

-- | Shift a coordinate in a direction
shift :: Coor -> Direction -> Coor
shift c o = c + coor # o


--------------------------------------------------------------------------------
-- $slices
--
-- How to slice through a grid in a direction

slice :: ()
=> Coor      -- ^ Origin of the slice
-> Coor      -- ^ Size of the area to slice
-> Direction -- ^ Direction of the slice
-> [Coor]    -- ^ Coordinates that compose the slice
slice c dim dir = undefined
