module Day12

where

import Prelude

import Coor

import RIO.State
import RIO.Text (justifyLeft)

--------------------------------------------------------------------------------
-- | Setup the basic types



-- | The 3 types of instructions
data Instruction
  = Shift Direction Int -- ^ Shift position by some vector
  | Advance Int         -- ^ Proceed in the heading by some distance
  | Rotate Int          -- ^ Change the heading by n clockwise turns of 45d
  deriving (Eq, Show)

-- | Read an instruction from text
instrP :: Parser Instruction
instrP = choice
  [ Shift U <$> (char 'N' *> intP)
  , Shift R <$> (char 'E' *> intP)
  , Shift D <$> (char 'S' *> intP)
  , Shift L <$> (char 'W' *> intP)
  , Advance <$> (char 'F' *> intP)
  , Rotate         . (`div` 45) <$> (char 'R' *> intP)
  , Rotate . (8 -) . (`div` 45) <$> (char 'L' *> intP)
  ]

-- | Load the instructions from file
loadInstr :: IO [Instruction]
loadInstr = parseDat "day12_instructions.txt" $ some (lexeme instrP)


--------------------------------------------------------------------------------
-- | Solve the first problem

-- | The ship with its position and orientation
data Ship = Ship
  { _pos1    :: Coor
  , _heading :: Direction
  } deriving (Eq, Show)
makeLenses ''Ship

class HasPos a where
  pos :: Lens' a Coor

instance HasPos Ship where pos = pos1

-- | Apply an instruction to a ship
apply1 :: Instruction -> Ship -> Ship
apply1 (Shift d n) s = s & pos     %~ shift n d
apply1 (Advance n) s = s & pos     %~ shift n (s^.heading)
apply1 (Rotate  n) s = s & heading %~ rotate n

-- | Run a series of instructions
run1 :: [Instruction] -> Ship
run1 xs = execState (mapM_ (modify . apply1) xs) (Ship (0, 0) R)

-- | Solve the problem
solve1 :: IO Int
solve1 = run1 <$> loadInstr >>= \(Ship (x, y) _) -> pure (abs x + abs y)

-- >>> solve1
-- 2228

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | The second representation of a ship with its waypoint
data Ship2 = Ship2
  { _pos2 :: Coor
  , _wp   :: Coor
  } deriving (Eq, Show)
makeLenses ''Ship2

instance HasPos Ship2 where pos = pos2

-- | Apply an instruction to the second ship
apply2 :: Instruction -> Ship2 -> Ship2
apply2 (Shift d n) s = s & wp  %~ shift n d
apply2 (Advance n) s = s & pos %~ add (mult n $ s^.wp)
apply2 (Rotate n)  s = s & wp  %~ pivot (n `div` 2)

-- | Run a series of instructions
run2 :: [Instruction] -> Ship2
run2 xs = execState (mapM_ (modify . apply2) xs) (Ship2 (0, 0) (10, 1))

-- | Run the solution
solve2 :: IO Int
solve2 = run2 <$> loadInstr >>= \(Ship2 (x, y) _) -> pure (abs x + abs y)

-- >>> solve2
-- 42908
