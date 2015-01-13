module Foster.Interpreter
    ( interpret
    ) where

import Foster.IO (writeUnsolvedPuzzle, readUnsolvedPuzzle, writeSolvedPuzzle)
import Foster.Generator (generatePuzzle)
import Foster.Solver (solvePuzzle)

generate :: (Int, Int) -> FilePath -> IO ()
generate siz out = do
    generatePuzzle siz >>= writeUnsolvedPuzzle out
    putStrLn $ "Puzzle correctly generated in " ++ out

solve :: FilePath -> FilePath -> IO ()
solve inp out = do
    readUnsolvedPuzzle inp >>= (writeSolvedPuzzle out . solvePuzzle)
    putStrLn $ "Puzzle solved in " ++ out

interpret :: [String] -> IO ()
interpret ["generate", w, h] = generate (read w, read h) "input.txt"
interpret ["generate", w, h, out] = generate (read w, read h) out
interpret ["solve", inp] = solve inp "output.txt"
interpret ["solve", inp, out] = solve inp out
interpret _ = error "Help text"
