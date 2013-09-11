import Text.Trifecta
import System.Environment

import Parser
import Solver

main :: IO ()
main = do
  [file] <- getArgs
  res <- parseFromFileEx dimacs file
  case res of
    Failure perr -> print perr
    Success cnf  -> do
      case dpll cnf of
        (model: _) -> print model
        [] -> putStrLn "unsat"
