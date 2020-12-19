module Prelude
  ( module X

  , Parser
  , loadDat
  , parseDat
  , prs

  , sc
  , lexeme
  , intP
  , symbol
  , sepBy2
 
  , xor
  , count
  , inBounds

  , putStrLn
  )
where

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~), try
  )

import Text.Megaparsec as X hiding
  (many, some, noneOf, count, State, pos1)

import RIO.Partial as X (fromJust)
import RIO.List.Partial as X (head)

import Control.Arrow                         as X ((***), (|||))
import Control.Lens                          as X
import Text.Megaparsec.Char                  as X
import System.IO                             as X (print)

-- Only to define utils
import qualified RIO.Text                    as T
import qualified Text.Megaparsec.Char.Lexer  as L
import qualified System.IO                   as SO (putStrLn)


--------------------------------------------------------------------------------
-- | Datafile helpers

-- | Complete the filepath to a dat-file
datfile :: Text -> FilePath
datfile = ("/home/david/aoc/dat/" <>) . T.unpack

-- | Load raw text from a file in the `dat` folder
loadDat :: Text -> IO Text
loadDat = readFileUtf8 .  datfile

-- | Run a parser on a file in the `dat` folder and return the result
parseDat :: Text -> Parser a -> IO a
parseDat f p = prs p <$> loadDat f

--------------------------------------------------------------------------------
-- | Parsing helpers

-- | Base type for parsers
type Parser = Parsec Void Text

-- | Consume whitespace
sc :: Parser ()
sc = L.space space1 empty empty

-- | Turn a parser into one that consumes whitespace after it succeeds
lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Parse an integer
intP :: Parser Int
intP = fromJust . readMaybe <$> some digitChar

-- | Parse a symbol
symbol :: Text -> Parser ()
symbol = void . L.symbol sc

-- | Parse multiple occurences of parser `a` separated by `b` with at least 2
-- a's and 1 b occurring.
sepBy2 :: Parser a -> Parser b -> Parser [a]
sepBy2 a sep = try $ (:) <$> a <*> some (try $ sep *> a)

   -- sepBy2 intP (char ' ') --((:) <$> intP <*> many (try $ char ' ' *> intP))
-- | Force run a parser, error on fail
prs :: Parser a -> Text -> a
prs p txt = case parse p "" txt of
  Left e  -> error . errorBundlePretty $ e
  Right x -> x

--------------------------------------------------------------------------------
-- | Little utilities

-- | Boolean exclusive-or
xor :: Bool -> Bool -> Bool
xor True False = True
xor False True = True
xor _ _        = False

-- | Count how often a predicate is true on a foldable of things
count :: Foldable t => (a -> Bool) -> t a -> Int
count p = sum . map (\a -> if p a then 1 else 0) . toList

-- | Return whether a value falls within 2 bounds (inclusive)
inBounds :: Ord a => a -> a -> a -> Bool
inBounds x lower upper = lower <= x && x <= upper

-- | Friendlier putStrLn from System.IO
putStrLn :: MonadIO m => Text -> m ()
putStrLn = liftIO . SO.putStrLn . T.unpack
