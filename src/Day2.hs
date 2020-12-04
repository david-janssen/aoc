module Day2

where

import Prelude

import RIO.List.Partial ((!!))

import qualified RIO.Text as T
import qualified RIO.Text.Partial as T (count)
import qualified Text.Megaparsec.Char.Lexer as L

--------------------------------------------------------------------------------
-- | Define the data-structure and parsers

-- | 1 Entry in the password file
data Entry = Entry
  { _password :: Text -- ^ The stored password
  , _minCount :: Int  -- ^ The minimum amount of times a char must occurs
  , _maxCount :: Int  -- ^ The maximum amount of times a char may occur
  , _target   :: Char -- ^ The target character to check for
  } deriving Show
makeLenses ''Entry

-- | How to read 1 line of the password file
entryP :: Parser Entry
entryP = do
  mn <- L.decimal <* char '-'
  mx <- L.decimal <* char ' '
  tg <- anySingle <* string ": "
  ps <- someTill anySingle (eof <|> void newline)
  pure $ Entry (T.pack ps) mn mx tg

-- | Return whether the provided Entry password is valid according to its rules
isValid :: Entry -> Bool
isValid e = e^.minCount <= n && n <= e^.maxCount
  where
    n :: Int
    n = T.count (T.singleton $ e^.target) $ e^.password

--------------------------------------------------------------------------------
-- | Solve the puzzle

-- | The list of entries as parsed from the file
entries :: IO [Entry]
entries = parseDat "day2_passwords.txt" $ many entryP

-- | Count the valid entries
solve :: IO Int
solve = length . filter isValid <$> entries

-- >>> solve
-- 528

--------------------------------------------------------------------------------
-- | Solve the second puzzle

-- | Alternate way of validating a password entry
isValid2 :: Entry -> Bool
isValid2 e = let p = T.unpack $ e^.password
                 a = p !! (e^.minCount - 1)
                 b = p !! (e^.maxCount - 1)
             in (a == e^.target) `xor` (b == e^.target)

-- | Count the valid entries
solve2 :: IO Int
solve2 = length . filter isValid2 <$> entries

-- >>> solve2
-- 497
