module Foster.Checker where

import Foster.Data
import Foster.Solver (solvePuzzle)

check :: UnsolvedPuzzle -> SolvedPuzzle -> Bool
check unPuz solPuz = solvePuzzle unPuz == solPuz
