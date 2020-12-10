module Day8

where

import Prelude

import RIO.State
import RIO.Vector.Partial as V ((!), update)

import qualified RIO.Vector.Boxed   as V


--------------------------------------------------------------------------------
-- Define the data representation

-- | The different instructions that exist in a program
data Instruction
  = Acc Int
  | Jmp Int
  | Nop Int -- Don't know why 'Nop' get's an Int in the data, but it does
  deriving (Eq, Show)

-- | A series of instructions
type Program = V.Vector Instruction

-- | Parse an instruction
instructionP :: Parser Instruction
instructionP = do
  f <- choice [ Acc <$ symbol "acc"
              , Jmp <$ symbol "jmp"
              , Nop <$ symbol "nop" ]
  s <- True <$ symbol "+" <|> False <$ symbol "-"
  n <- intP
  _ <- void eol <|> eof

  pure $ f (if s then n else -n)

-- | Load the program from disc
loadProgram :: IO Program
loadProgram = V.fromList <$> (parseDat "day8_assembly.txt" $ some instructionP)

--------------------------------------------------------------------------------
-- Solve the problem

-- | The App-state during program execution
data App = App
  { _acc  :: Int
  , _hist :: [Int]
  , _pos  :: Int }
  deriving (Eq, Show)
makeLenses ''App

-- | The result of executing a program
data Result = Looped Int | Done Int
  deriving (Eq, Show)

-- | Step the program once
step :: Program -> State App ()
step ps = do
  i <- use pos
  hist <>= [i]
  case ps ! i of
    Acc n -> pos += 1 >> acc += n
    Jmp n -> pos += n
    Nop _ -> pos += 1

-- | Step the program until termination
run :: Program -> Result
run p = evalState go (App 0 [] 0)
  where
    len = V.length p
    go = do i <- use pos
            h <- use hist
            if | i `elem` h -> Looped <$> use acc
               | i == len   -> Done   <$> use acc
               | otherwise  -> step p >> go

-- | Put it all together
solve :: IO Result
solve = run <$> loadProgram

-- >>> solve
-- Looped 1723


--------------------------------------------------------------------------------
-- Solve the second problem

-- | List of all programs where 1 'Nop' is changed to a 'Jmp' or vice-versa
alts :: Program -> [Program]
alts p = catMaybes . map fix $ zip [0..] (V.toList p)
  where
    fix (_, (Acc _)) = Nothing
    fix (i, (Nop n)) = Just . update p $ V.fromList [(i, (Jmp n))]
    fix (i, (Jmp n)) = Just . update p $ V.fromList [(i, (Nop n))]

solve2 :: IO [Result]
solve2 = filter p . map run . alts <$> loadProgram
  where p (Done _) = True
        p _        = False
