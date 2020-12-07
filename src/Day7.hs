module Day7

where

import Prelude

import qualified RIO.HashMap as M
import qualified RIO.Text    as T

--------------------------------------------------------------------------------
-- | Define the data representations and parsers

-- | Every kind of bag is identified by 2 descriptors
type BagId = (Text, Text)

-- | Contents decribes how many of certain bag-id's must be present
type Contents = M.HashMap BagId Int

-- | Rules about contents for each bag-id
type Rules = M.HashMap BagId Contents

-- | Parse a BagId
bagP :: Parser BagId
bagP = do
  a <- someTill lowerChar spaceChar
  b <- someTill lowerChar spaceChar
  _ <- string "bag" *> optional (char 's')
  pure (T.pack a, T.pack b)

-- | Parse a set of contents-descriptions
contentsP :: Parser Contents
contentsP = do
  let doOne = do
        n <- lexeme intP
        b <- lexeme bagP
        pure (b, n)
  let doNone = do
        void $ string "no other bags"
  (M.fromList <$> sepBy1 doOne (string ", ")) <|> (M.empty <$ doNone)

-- | Parse a single rule
ruleP :: Parser (BagId, Contents)
ruleP = do
  src <- lexeme bagP
  _   <- symbol "contain"
  cts <- contentsP
  _   <- symbol "."
  pure $ (src, cts)

-- | Read all the rules from file
loadRules :: IO Rules
loadRules = M.fromList <$> parseDat "day7_bags.txt" (some ruleP)

-- | For a set of rules, return a fold from BagId to *all* the bags inside it
contents :: Rules -> BagId -> [(BagId, Int)]
contents rs b = case M.lookup b rs of
  Just c  -> M.toList c <> concatMap go (M.toList c)
  Nothing -> []
  where go (b, n) = map (second (* n)) $ contents rs b

--------------------------------------------------------------------------------
-- | Solve the problem

-- | Count the bags that contain at least 1 "shiny gold" bag
solve :: IO Int
solve = do
  rs <- loadRules
  let p b = (("shiny", "gold") `elem`) . map fst $ contents rs b
  pure $ count p (M.keys rs)

-- >>> solve
-- 248

--------------------------------------------------------------------------------
-- | Solve the second problem

solve2 :: IO Int
solve2 = do
  rs <- loadRules
  pure . sum . map snd $ contents rs ("shiny", "gold")
