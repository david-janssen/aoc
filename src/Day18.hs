{-# OPTIONS_GHC -Wno-all #-}
module Day18

where

import Prelude hiding (Token)

import RIO.List (zip3)

import qualified RIO.Text as T

--------------------------------------------------------------------------------
-- | Solve the first expression
--
-- Not sure if lazy, but since we're completely left-associative in this problem
-- I just flip the expression and then parse right-associative recursively.

-- | The different terms in our maths language
data Expr = Add Expr Expr -- ^ Add two expressions
          | Mul Expr Expr -- ^ Multiple two expressions
          | Lit Int       -- ^ An integer
          deriving (Eq, Show)

-- | Evaluate an expression until it is resolved
eval :: Expr -> Int
eval (Lit n)   = n
eval (Mul a b) = eval a * eval b
eval (Add a b) = eval a + eval b

elemP :: Parser Expr
elemP = litP <|> pared exprP

mulP :: Parser Expr
mulP = Mul <$> (elemP <* symbol " * ") <*> exprP

addP :: Parser Expr
addP = Add <$> (elemP <* symbol " + ") <*> exprP

litP :: Parser Expr
litP = Lit <$> intP

pared :: Parser a -> Parser a
pared p = char '(' *> p <* char ')'

exprP :: Parser Expr
exprP = choice . map try $ [mulP, addP, litP, pared exprP]

readF :: Text -> Expr
readF = prs exprP . flipF

flipF :: Text -> Text
flipF = T.map go . T.reverse
  where go '(' = ')'
        go ')' = '('
        go a   = a

exprs :: IO [Expr]
exprs = map readF . T.lines <$> loadDat "day18_maths.txt"

solve :: IO Int
solve = sumOf (folded.to eval) <$> exprs

--------------------------------------------------------------------------------
-- | Solve the second problem
--
-- Now we can't parse as cleanly because we do not know the tree-structure until
-- we finish reading the entire thing, so instead we tokenize first and then
-- eval differently.

data Token = TLit   Int
           | TAdd
           | TMul
           | TParen [Token]
           deriving (Eq, Show)
makePrisms ''Token

tokenP :: Parser Token
tokenP = choice
  [ TLit   <$> intP
  , TAdd   <$  char '+'
  , TMul   <$  char '*'
  , TParen <$> pared texprP ]

texprP :: Parser [Token]
texprP = sepBy1 tokenP $ char ' '

teval :: [Token] -> Int
teval = go 1 . map depar
  where
    go acc ((TLit a):TAdd:(TLit b):stck) = go acc ((TLit $ a + b):stck)
    go acc ((TLit b):TMul:stck)          = go (b * acc) stck
    go acc ((TLit a):[])                 = a * acc
    go _   _                             = error "Malformed token expression"

    depar = \case (TParen ts) -> TLit (teval ts)
                  t           -> t

exprs2 :: IO [[Token]]
exprs2 = parseDat "day18_maths.txt" $ some (lexeme texprP)

solve2 :: IO Int
solve2 = sumOf (folded.to teval) <$> exprs2
