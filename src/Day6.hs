module Day6

where

import Prelude

import Data.Char
import RIO.List.Partial (foldl1)
import qualified RIO.Set as S

--------------------------------------------------------------------------------
-- | Define the data representations

-- | The set of questions to which 1 person answered yes
type Answers = S.Set Char

-- | The collection of answers for all people in 1 group
type Group = [Answers]

-- | How to parse a line of answers from Text
answersP :: Parser Answers
answersP = S.fromList <$> someTill (satisfy isLetter) (void newline <|> eof)

-- | How to parse a group of answers
groupP :: Parser Group
groupP = someTill answersP (void newline <|> eof)

groups :: IO [Group]
groups = parseDat "day6_customs.txt" $ some groupP


--------------------------------------------------------------------------------
-- | Solve the problem

-- | Calculate the sum of the questions to which at least 1 person answered yes
-- over groups.
solve :: IO Int
solve = sum . map (S.size . mconcat) <$> groups

--------------------------------------------------------------------------------
-- | Solve the second problem

-- | Calculate the sum of the questions to which everyone answered yes over groups
solve2 :: IO Int
solve2 = sum . map (S.size . (foldl1 S.intersection)) <$> groups
