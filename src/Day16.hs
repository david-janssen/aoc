module Day16

where

import Prelude

import Control.Monad (replicateM)
import RIO.List (transpose, sortOn)
import RIO.List.Partial (foldl1')

import qualified RIO.HashMap as M
import qualified RIO.Text as T
import qualified RIO.Set as S

--------------------------------------------------------------------------------
-- | Define the basic types

-- | Basic type of a 'Rule', which is just 2 ranges, represented as 4 numbers
type Rule = (Int, Int, Int, Int)

-- | We store a bunch of rules by name
type Rules = M.HashMap Text Rule

-- | All tickets are exactly 20 numbers, ideally we'd encode this, but meh
type Ticket = [Int]

-- | Collect all configuration values into a record
data Config = Config
  { _rules :: Rules
  , _mine  :: Ticket
  , _oths  :: [Ticket]
  } deriving (Eq, Show)
makeLenses ''Config

-- | Parse the name of a rule from text
nameP :: Parser Text
nameP = T.pack <$> someTill (letterChar <|> char ' ') (string ": ")

-- | Parse a 'Rule' from text
ruleP :: Parser Rule
ruleP = do
  a <- intP <* char '-'
  b <- intP <* string " or "
  c <- intP <* char '-'
  (a,b,c,) <$> intP

lineP :: Parser (Text, Rule)
lineP = (,) <$> nameP <*> ruleP

-- | Parse a collection of named rules
rulesP :: Parser Rules
rulesP = M.fromList <$> some (lineP <* eol)

-- | Parse a ticket
ticketP :: Parser Ticket
ticketP = sepBy1 intP (char ',')

-- | Parse the entire file, returning rules, my ticket, and the tickets nearby
configP :: Parser Config
configP = do
  rs <- lexeme rulesP
  _  <- lexeme $ string "your ticket:"
  t  <- lexeme ticketP
  _  <- lexeme $ string "nearby tickets:"
  os <- some $ lexeme ticketP
  pure $ Config rs t os

-- | Load the entire config from disc
loadConfig :: IO Config
loadConfig = parseDat "day16_rules.txt" configP

-- | Load the example from disc
loadExample :: IO Config
loadExample = parseDat "day16_example.txt" configP

-- test ::

--------------------------------------------------------------------------------
-- | Solve the first problem

-- | Return whether some value could fall within some rule
fits :: Rule -> Int -> Bool
fits (a, b, c, d) i = (a <= i && i <= b) || (c <= i && i <= d)

-- | Return if some int falls in any of the provided rules
fitsAny :: Rules -> Int -> Bool
fitsAny rs i = any (flip fits i) $ M.elems rs

-- | Solve the problem
solve :: IO Int
solve = do
  cfg <- loadConfig
  pure . sum . filter (not . fitsAny (cfg^.rules)) $ concat (cfg^.oths)

-- >>> solve
-- 27898

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Remove all tickets that contain at least 1 impossible value
clean :: Rules -> [Ticket] -> [Ticket]
clean rs = filter (all $ fitsAny rs)

-- | Return a set of all possible rules that fit some value
validNames :: Rules -> Int -> S.Set Text
validNames rs i = M.foldlWithKey' go S.empty rs
  where go acc k v = if fits v i then S.insert k acc else acc

-- | Return a set of rules that could apply to all provided numbers
validForAll :: Rules -> [Int] -> S.Set Text
validForAll rs = foldl1' S.intersection . map (validNames rs)

-- | Pop the first element to satisfy some predicate out of a list
popWith :: (a -> Bool) -> [a] -> Maybe (a, [a])
popWith p xs = go xs []
  where
    go [] _      = Nothing
    go (x:xs) hd = if p x then Just (x, reverse hd <> xs) else go xs (x:hd)

-- | Recursively constrain a list of sets, trying to fill in each item uniquely
constrain :: Ord a => [S.Set a] -> [S.Set a]
constrain src = map snd . sortOn fst $ go (zip [0..] src) []
  where
    go [] done = done
    go xs done = case popWith ((1==) . S.size . snd) xs of
      Nothing      -> xs <> done
      Just (x, xs') -> go (map prune xs') (x:done)
        where prune y = over _2 (S.delete (head . toList . snd $ x)) y

solve2 :: IO Int
solve2 = do
  -- Load config
  (Config rs me os') <- loadConfig
  let os = clean rs os'

  -- Find valid rules for each cell
  let vs = map (validForAll $ rs) (transpose os)

  -- Constrain the rules uniquely and combine with my own ticket
  let m = M.fromList $ zip (map (head . toList) $ constrain vs) me

  -- Find all keys that start with departure and multiply them
  let m' = M.filterWithKey (\k _ -> T.isPrefixOf "departure" k) m
  pure . product . M.elems $ m'

-- >>> solve2
-- 2766491048287
