module Day14

where

import Prelude

import Data.Bits
import RIO.List (repeat)
import RIO.Seq (replicateA)
import RIO.State

import qualified RIO.Vector.Boxed as V
import qualified RIO.Text         as T
import qualified RIO.HashMap      as M

--------------------------------------------------------------------------------
-- | Define the basic types


-- | Addresses into memory are just ints
type Addr  = Int

-- | We represent bitmasks as a vector of maybe a mask
newtype Mask = Mask { unMask :: V.Vector (Maybe Bool)} deriving (Eq, Show)

-- | CPU instructions
data Instr = SetMask  Mask
           | WriteVal Addr Int
  deriving (Eq, Show)

-- | Parse a single bit
bitP :: Parser (Maybe Bool)
bitP = Nothing    <$ char 'X'
   <|> Just True  <$ char '1'
   <|> Just False <$ char '0'
  
-- | Parse a mask from a series of 'X', '1', and '0' entries
maskP :: Parser Mask
maskP = Mask . V.reverse . V.fromList . toList <$> replicateA 36 bitP

-- | Parse a line of instruction
instrP :: Parser Instr
instrP = mskP <|> memP
  where mskP = SetMask <$> (string "mask = " *> maskP)
        memP = do
          _    <- string "mem["
          addr <- intP
          _    <- string "] = "
          val  <- intP
          pure $ WriteVal addr val

-- | Load the instructions from disk
loadInstr :: IO [Instr]
loadInstr = parseDat "day14_instr.txt" $ some (lexeme instrP)

-- | Pretty-print a mask back into its Text representation
ppMask :: Mask -> Text
ppMask = T.pack . toList . V.map go . unMask
  where go Nothing      = 'X'
        go (Just True)  = '1'
        go (Just False) = '0'

-- | Pretty-print an instruction
ppInstr :: Instr -> Text
ppInstr (SetMask m) = ppMask m
ppInstr (WriteVal a i) = T.justifyLeft 6 ' ' (tshow a) <> " : " <> tshow i

-- | Shorthand
pp :: Instr -> IO ()
pp = putStrLn . ppInstr

-- | Experimentation purposes
foo :: Mask
foo = prs maskP "0010011010X1000100X101011X10010X1010"

ex1 :: Mask
ex1 = prs maskP "000000000000000000000000000000X1001X"

ex2 :: Mask
ex2 = prs maskP "00000000000000000000000000000000X0XX"

--------------------------------------------------------------------------------
-- | Setup the computation to solve the first problem

-- | Calculate the transformed int after all the bits have been set by the mask
apply :: Mask -> Int -> Int
apply (Mask ms) n = fst $ V.foldl' go (n, 0) ms
  where go (n', i) Nothing = (n', i + 1)
        go (n', i) (Just True) = (setBit n' i, i + 1)
        go (n', i) (Just False) = (clearBit n' i, i + 1)

-- | The execution environment
data Env = Env
  { _mem :: M.HashMap Int Int
  , _msk :: Mask
  } deriving (Eq, Show)
makeLenses ''Env

-- | The initial empty environment
defEnv :: Env
defEnv = Env M.empty undefined

-- | Do 1 stateful computation
step :: Instr -> State Env ()
step (SetMask m)    = msk .= m
step (WriteVal a i) = use msk >>= \m -> mem %= M.insert a (apply m i)

-- | Run a simulation and extract the sum across memory at the end
run :: State Env () -> Int
run = sum . M.elems . _mem . flip execState defEnv

-- | Solve the problem
solve :: IO Int
solve = run . mapM_ step <$> loadInstr

-- >>> solve
-- 4886706177792

--------------------------------------------------------------------------------
-- | Setup the computation to solve the second problem

-- | Calculate all address locations caused by applying a mask to an address
mangle :: Mask -> Addr -> [Addr]
mangle (Mask ms) a = go [a] $ zip [0..] (toList ms)
  where
    go acc [] = acc
    go acc ((_, Just False):rst) = go acc rst
    go acc ((i, Just True):rst)  = go (map (flip setBit i) acc) rst
    go acc ((i, Nothing):rst)    = let acc' = map (flip setBit i) acc
                                           <> map (flip clearBit i) acc
                                   in go acc' rst

-- | Do 1 stateful computation
step2 :: Instr -> State Env ()
step2 (SetMask m)    = msk .= m
step2 (WriteVal a i) = do
  as <- flip mangle a <$> use msk
  mem %= (M.fromList (zip as $ repeat i) `M.union`)

-- | Solve the second problem
solve2 :: IO Int
solve2 = run . mapM_ step2 <$> loadInstr

-- >>> solve2
-- 3348493585827
