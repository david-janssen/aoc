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

t1, t2, t3 :: Text
t1 = "26: \"a\""
t2 = "64: 16 15 | 26 21"
t3 = "125: 16 16"

foo :: IO ()
foo = do
  (r, ms) <- parseDat "day19_example.txt" fileP
  print r
  mapM_ print ms



match :: Rule -> Msg -> Bool
match (Mat A) [A] = True
match (Mat B) [B] = True
