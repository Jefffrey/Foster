module Foster.Interpreter
    ( interpret
    ) where

import Foster.IO (writeUnsolvedPuzzle, readUnsolvedPuzzle, writeSolvedPuzzle)
import Foster.Generator (generatePuzzle)
import Foster.Solver (solvePuzzle)
import Data.List (intercalate)

generate :: (Int, Int) -> FilePath -> IO ()
generate siz out = do
    generatePuzzle siz >>= writeUnsolvedPuzzle out
    putStrLn $ "Puzzle correctly generated in " ++ out

solve :: FilePath -> FilePath -> IO ()
solve inp out = do
    readUnsolvedPuzzle inp >>= (writeSolvedPuzzle out . solvePuzzle)
    putStrLn $ "Puzzle solved in " ++ out

helpText :: String
helpText = 
    intercalate "\n"
        [ "usage: foster <command>\n"
        , "Here's a list of commands:"
        , "    generate\tGenerates a new puzzle"
        , "    solve\tSolves an existing puzzle"
        , "    check\tChecks if a solution to a puzzle is correct"
        , "    help\tPrints this message" ]

generateHelpText :: String
generateHelpText = "usage: foster generate <width> <height> [output=input.txt]"

solveHelpText :: String
solveHelpText = "usage: foster solve <input> [output=output.txt]"

checkHelpText :: String
checkHelpText = "usage: foster check [input=input.txt] [output=output.txt]"

parseError :: String
parseError = "Unrecognized command\n"

interpret :: [String] -> IO ()
interpret ["generate", w, h] = generate (read w, read h) "input.txt"
interpret ["generate", w, h, out] = generate (read w, read h) out
interpret ["solve", inp] = solve inp "output.txt"
interpret ["solve", inp, out] = solve inp out
interpret ("generate":_) = putStrLn $ parseError ++ generateHelpText
interpret ("solve":_) = putStrLn $ parseError ++ solveHelpText
interpret ("check":_) = putStrLn $ parseError ++ checkHelpText
interpret ("help":_) = putStrLn helpText
interpret _ = putStrLn $ parseError ++ helpText -- @todo: error stream?
