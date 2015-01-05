module Main where

import System.Environment (getArgs)
import Foster.IO (readUnsolvedPuzzle, writeSolvedPuzzle)
import Foster.Solver (solvePuzzle)

main :: IO ()
main = do
    [inputPath, outputPath] <- getArgs
    unPuz <- readUnsolvedPuzzle inputPath
    writeSolvedPuzzle (solvePuzzle unPuz) outputPath
