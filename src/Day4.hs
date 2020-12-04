module Day4

where

import Prelude

import Data.Char (isHexDigit)

import qualified RIO.Text          as T
import qualified RIO.Text.Partial  as T
import qualified RIO.Set           as S
import qualified RIO.HashMap       as M


--------------------------------------------------------------------------------
-- | Define the data representation and parsers

-- | We represent Key,Value pairs as tuples of Text
type Pair = (Text, Text)

-- | We represent passports as lists of pairs
type Passport = [Pair]

-- | How to parse 1 pair
pairP :: Parser Pair
pairP = do
  k <- someTill anySingle (void $ char ':')
  v <- someTill anySingle (void (char ' ') <|> void newline <|> eof)
  pure (T.pack k, T.pack v)

-- | How to parse 1 passport
passportP :: Parser Passport
passportP = someTill pairP (void newline <|> eof)

-- | The list of passports as parsed from the file
passports :: IO [Passport]
passports = parseDat "day4_passports.txt" $ some passportP


--------------------------------------------------------------------------------
-- | Solve the problem

-- | The required fields for a passport to be valid
required :: S.Set Text
required = S.fromList $ ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- | Check if the passport is valid
isValid :: Passport -> Bool
isValid p = let keys = S.fromList $ p ^.. folded . _1 in
  case required `S.difference` keys of
    x | S.null x       -> True
      | S.size x == 1  -> "cid" `S.member` x
      | otherwise      -> False

-- | Count the valid passports
solve :: IO Int
solve = count isValid <$> passports

-- >>> solve
-- 204


--------------------------------------------------------------------------------
-- | Solve the second problem


-- | Check if the passport is valid
isValid2 :: Passport -> Bool
isValid2 p = isValid p && and (map check p)
  where
    -- Read an int
    readInt t = readMaybe (T.unpack t) :: Maybe Int
   
    -- Read an int and check for bounds
    boundedNum t low hi = case readInt t of
      Just i  -> inBounds i low hi
      Nothing -> False

    -- Simple parsing and checking years within a range
    check ("byr", a) = boundedNum a 1920 2002
    check ("iyr", a) = boundedNum a 2010 2020
    check ("eyr", a) = boundedNum a 2020 2030

    -- Allow for 2 suffixes and ajust the allowed ranges based on those
    check ("hgt", a) = case T.take 2 $ T.reverse a of
      "ni" -> boundedNum (T.take 2 a) 59 76   && T.length a == 4
      "mc" -> boundedNum (T.take 3 a) 150 193 && T.length a == 5
      _    -> False

    -- Allow only a 6 digit hex-string prefixed by a '#'
    check ("hcl", a) = T.length a == 7
                    && T.head a == '#'
                    && all isHexDigit (T.unpack $ T.drop 1 a)

    -- Simple check for fixed values
    check ("ecl", a) = a `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

    -- Exactly 9 elements and must evaluate to an Int
    check ("pid", a) = T.length a == 9 && isJust (readInt a)

    -- Everything else (only "cid", in practice) is fine
    check _ = True

-- | Count the valid passports
solve2 :: IO Int
solve2 = count isValid2 <$> passports

-- >>> solve
-- 179
