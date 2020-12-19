module Day19

where

import Prelude hiding (Token, match)

import qualified RIO.HashMap as M

-- | Elements of a message are either A or B
data Bit = A | B deriving (Eq, Show)

-- | Messages are simply lists of bits
type Msg = [Bit]

-- | Different types of rules
data Rule
  = Mat Bit     -- ^ Match 'A' or 'B'
  | Ser [Rule]  -- ^ Match a series of other rules in sequence
  | Opt [Rule]  -- ^ Match one of a collection of rules
  deriving (Eq, Show)

--------------------------------------------------------------------------------
-- | Parse the data

-- | We first parse rule-tokens, and then construct the network once all the
-- tokens have been read from text.
data Token
  = TMat Bit
  | TSer [Int]
  | TOpt [Token]
  deriving (Eq, Show)


-- | Parse a single, non recursive (TOpt) token
simpleP :: Parser Token
simpleP = choice . map try $
  [ TMat A <$  (string "\"a\"")
  , TMat B <$  (string "\"b\"")
  , TSer   <$> ((:) <$> intP <*> many (try $ char ' ' *> intP))
  ]

-- | Parse either a simple token, or a compound combination of simple tokens
tokenP :: Parser Token
tokenP = TOpt <$> sepBy2 simpleP (symbol " | ") <|> simpleP

-- | Parse a line containing a numbered rule
lineP :: Parser (Int, Token)
lineP = (,) <$> (intP <* symbol ": ") <*> tokenP

-- | Assemble a list of numbered rules tokens into a network starting at some rule
assemble :: [(Int, Token)] -> Int -> Rule
assemble rs = go . lookup
  where
    lookup i = fromJust $ M.lookup i (M.fromList rs)
    f        = map (go . lookup)
    go (TMat b)  = Mat b
    go (TSer ts) = Ser $ f ts
    go (TOpt ts) = Opt $ map go ts

-- | Parse a message from text
msgP :: Parser Msg
msgP = some $ (A <$ char 'a') <|> (B <$ char 'b')

-- | How to parse the entire file
fileP :: Parser (Rule, [Msg])
fileP = do
  ts <- some $ lexeme lineP
  ms <- some $ lexeme msgP
  pure $ (assemble ts 0, ms)


load :: IO (Rule, [Msg])
load = parseDat "day19_rules.txt" fileP

expl :: IO (Rule, [Msg])
expl = parseDat "day19_example.txt" fileP

--------------------------------------------------------------------------------

-- | Turn a message into all the possible lists of messages left by consuming
-- all matching input for some rule.
--
-- NOTE: [] would signify failure, whereas [[]] signifies success.
munch :: Rule -> Msg -> [Msg]
munch (Mat A) (A:msg)   = [msg]
munch (Mat B) (B:msg)   = [msg]
munch (Ser []) msg      = [msg]
munch (Ser (r:rs)) msg  = concatMap (munch (Ser rs)) (munch r msg)
munch (Opt rs) msg      = concatMap (flip munch msg) rs
munch _ _               = []

solve :: IO Int
solve = do
  (r, ms) <- parseDat "day19_rules.txt" fileP
  pure . count (any null) . map (munch r) $ ms

solve2 :: IO Int
solve2 = do
  (r, ms) <- parseDat "day19_rules2.txt" fileP
  pure . count (any null) . map (munch r) $ ms
