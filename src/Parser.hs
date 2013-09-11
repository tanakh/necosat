module Parser (
  dimacs,
  CNF, Clause, Lit, Var,
  ) where

import           Control.Applicative
import           Control.Monad
import           Text.Trifecta

type CNF = [Clause]
type Clause = [Lit]
type Lit = Int
type Var = Int

dimacs :: Parser CNF
dimacs = do
  skipMany $ char 'c' >> manyTill anyChar newline
  (_nvar, nclause) <- (,) <$ symbol "p" <* symbol "cnf" <*> integral <*> integral
  replicateM nclause $ manyTill integral $ (try $ integral >>= guard . (==0))
  where
    integral = fmap fromIntegral (spaces >> integer)
