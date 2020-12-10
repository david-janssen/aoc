module Prelude
  ( module X

  , Parser
  , loadDat
  , parseDat

  , sc
  , lexeme
  , intP
  , symbol
 
  , xor
  , count
  , inBounds
  )
where

import RIO as X hiding
  (-- Not the lens stuff, I want more support for lenses from "Control.Lens"
    view, ASetter, ASetter', Lens, Getting, Lens'
  , SimpleGetter, lens, over, set, sets, to, (^.)

    -- The following line is required for newer stack releases.
    -- This is also the reason for the OPTIONS_GHC pragma
  , (^..), (^?), preview, (%~), (.~)
  )

import Text.Megaparsec as X hiding
  (many, some, try, noneOf, count, State)

import RIO.Partial as X (fromJust)

import Control.Lens                          as X
import Text.Megaparsec.Char                  as X

-- Only to define utils
import qualified RIO.Text                    as T
import qualified Text.Megaparsec.Char.Lexer  as L


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
parseDat f p = do
  txt <- loadDat f
  case parse p "" txt of
    Left e  -> error (errorBundlePretty e)
    Right x -> pure x

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
