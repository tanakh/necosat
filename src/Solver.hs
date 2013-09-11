module Solver (
  dpll,

  -- cdcl,

  satisfy,
  ) where

import           Data.Bits
import           Data.List
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import           Parser

type Model = [Lit]

dpll :: CNF -> [Model]
dpll cnf
  | null cnf = [[]]
  | Just _ <- find null cnf = []
  | Just [l] <- find ((==1) . length) cnf =
    map (l:) $ dpll $ subst l cnf
  | Just (v, sig) <- find ((/=(3::Int)) . snd) $ Map.toList $ Map.fromListWith (.|.) $ map (\x -> (abs x, if x > 0 then 1 else 2)) $ concat cnf =
    dpll $ subst (v * if sig == 1 then 1 else -1) cnf
  | ((l:_):_) <- cnf =
    map ( l:) (dpll (subst l cnf)) ++
    map (-l:) (dpll (subst (-l) cnf))

subst :: Lit -> CNF -> CNF
subst l = map (filter $ (/=(-l))) . filter (l `notElem`)

satisfy :: CNF -> Model -> Bool
satisfy cnf model
  | Set.size smodel /= Set.size svar = False
  | otherwise = all isSat cnf
  where
    smodel = Set.fromList model
    svar   = Set.fromList (map abs model)
    isSat clause = all (`Set.member` smodel) clause

-- satFormula, unsatFormula :: CNF
-- satFormula = [[-1, 2], [1, -2]]
-- unsatFormula = [[1, 2], [1, -2], [-1, 2], [-1, -2]]
