module Foster.Interpreter
    ( interpret
    ) where

import Foster.IO (writeUnsolvedPuzzle, readUnsolvedPuzzle, writeSolvedPuzzle, showSolvedPuzzle)
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

check :: FilePath -> FilePath -> IO ()
check inp out = do
    unPuz <- readUnsolvedPuzzle inp
    solPuzStr1 <- readFile out
    let solPuzStr2 = showSolvedPuzzle . solvePuzzle $ unPuz
    let zipL = filter (\(_, b, c) -> b /= c) $ zip3 [1..] (lines solPuzStr1) (lines solPuzStr2)
    if null zipL
        then putStrLn $ "Puzzle in " ++ out ++ " is a solution to puzzle in " ++ inp
        else mapM_ (\(i, a, b) -> do
            putStrLn $ "Mismatch on line " ++ show (i :: Int) ++ ":"
            putStrLn $ "        " ++ a
            putStrLn "    Should be:"
            putStrLn $ "        " ++ b) zipL

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
parseError = "unrecognized command:\n"

interpret :: [String] -> IO ()
interpret ["generate", w, h] = generate (read w, read h) "input.txt"
interpret ["generate", w, h, out] = generate (read w, read h) out
interpret ["solve", inp] = solve inp "output.txt"
interpret ["solve", inp, out] = solve inp out
interpret ["check"] = check "input.txt" "output.txt"
interpret ["check", inp] = check inp "output.txt"
interpret ["check", inp, out] = check inp out 
interpret ("generate":_) = error $ parseError ++ generateHelpText
interpret ("solve":_) = error $ parseError ++ solveHelpText
interpret ("check":_) = error $ parseError ++ checkHelpText
interpret ("help":_) = putStrLn helpText
interpret _ = error $ parseError ++ helpText -- @todo: error stream?
