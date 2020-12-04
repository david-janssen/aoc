module Day3
  ( solve )
where

import Prelude

import RIO.List.Partial (head)
import Util             (loadDat)

import qualified RIO.Set  as S
import qualified RIO.Text as T


--------------------------------------------------------------------------------
-- Define the representation and interface of the terrain

-- | Coordinates are just tuples of Ints
type Coor  = (Int, Int)

-- | A description of the terrain across which we travel
data Terrain = Terrain
  { _trees  :: S.Set Coor -- ^ Set of coordinates that contain trees
  , _width  :: Int        -- ^ The width of 1 line of the pattern
  , _height :: Int        -- ^ The amount of lines in the pattern
  } deriving Show
makeLenses ''Terrain

-- | Parse a Terrain record from a `..#.#.` style representation
readTerrain :: Text -> Terrain
readTerrain t = let ls = lines $ T.unpack t in Terrain
  { _width  = length . head $ ls
  , _height = length ls
  , _trees  = S.fromList $ [ (r, c) | (r, l) <- zip [0..] ls,
                                      (c, x) <- zip [0..] l,
                                      x == '#' ]
  }

-- | Return wether the provided Coor contains a tree
hasTree :: Terrain -> Coor -> Bool
hasTree tr (r, c) = (r, c `mod` tr^.width) `elem` tr^.trees


--------------------------------------------------------------------------------
-- Define the walk and solve the problem

-- | Stepsize right for each line down
type Step = Int

-- | Given a Walk and some Trees, calculate the collisions
countBumps :: Step -> Terrain -> Int
countBumps w t =
  let walk = map (\r -> (r, w * r)) [0..t^.height]
  in  foldl' (\acc c -> if hasTree t c then acc + 1 else acc) 0 walk

-- | Load terrain from disc
loadTerrain :: IO Terrain
loadTerrain = readTerrain <$> loadDat "day3_terrain.txt"

-- | Put it all together
solve :: Step -> IO Int
solve n = countBumps n <$> loadTerrain

-- >>> solve 3
-- 181
