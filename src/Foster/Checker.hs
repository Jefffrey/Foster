module Foster.Checker where

import Foster.Solver (solvePuzzle)
import Foster.IO (readUnsolvedPuzzle, showSolvedPuzzle)
import Control.Monad (unless)

-- @todo: also give column and show only the important part of the row
check :: FilePath -> FilePath -> Bool -> IO ()
check inp out sil = do
    unPuz <- readUnsolvedPuzzle inp
    solPuzStr1 <- readFile out

    solPuz2 <- solvePuzzle sil unPuz
    let solPuzStr2 = showSolvedPuzzle solPuz2
    let zipL = filter (\(_, b, c) -> b /= c) $ zip3 [1..] (lines solPuzStr1) (lines solPuzStr2)
    if null zipL
        then unless sil . putStrLn $ "Solution is correct"
        else unless sil $
            mapM_ (\(i, a, b) -> do
                putStrLn $ "Mismatch on line " ++ show (i :: Int) ++ ":"
                putStrLn $ "        " ++ a
                putStrLn "    Should be:"
                putStrLn $ "        " ++ b ++ "\n") zipL
