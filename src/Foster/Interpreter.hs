{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Foster.Interpreter
    ( interpret
    ) where

import Foster.IO (writeUnsolvedPuzzle, readUnsolvedPuzzle, writeSolvedPuzzle, showSolvedPuzzle)
import Foster.Generator (generatePuzzle)
import Foster.Solver (solvePuzzle)
import Control.Applicative
import Control.Monad (unless)
import System.Console.CmdTheLine

type Size = (Int, Int)

instance ArgVal Size where
    converter = pair ','

instance ArgVal (Maybe Size) where
    converter = just

generate :: Size -> String -> FilePath -> Bool -> IO ()
generate (h, w) str out sil = do
    generatePuzzle (w, h) str >>= writeUnsolvedPuzzle out
    unless sil $
        putStrLn $ "Puzzle correctly generated in " ++ out

solve :: FilePath -> FilePath -> Bool -> IO ()
solve inp out sil = do
    readUnsolvedPuzzle inp >>= (writeSolvedPuzzle out . solvePuzzle)
    unless sil $
        putStrLn $ "Puzzle solved in " ++ out

-- @todo: also give column and show only the important part of the row
check :: FilePath -> FilePath -> Bool -> IO ()
check inp out sil = do
    unPuz <- readUnsolvedPuzzle inp
    solPuzStr1 <- readFile out
    let solPuzStr2 = showSolvedPuzzle . solvePuzzle $ unPuz
    let zipL = filter (\(_, b, c) -> b /= c) $ zip3 [1..] (lines solPuzStr1) (lines solPuzStr2)
    if null zipL
        then unless sil $
            putStrLn $ "Puzzle in " ++ out ++ " is a solution to puzzle in " ++ inp
        else unless sil $
            mapM_ (\(i, a, b) -> do
                putStrLn $ "Mismatch on line " ++ show (i :: Int) ++ ":"
                putStrLn $ "        " ++ a
                putStrLn "    Should be:"
                putStrLn $ "        " ++ b ++ "\n") zipL

silentArg :: Term Bool
silentArg = value . flag $ optInfo [ "silent", "s" ]

sizeArg :: Term Size 
sizeArg =
    required . pos 0 Nothing $
        posInfo
            { posName = "SIZE"
            , posDoc = "size defined as 'rows,columns';"
                    ++ " for example: 'foster generate 20,10'"
            }

stringArg :: Term String
stringArg =
    nonEmpty . opt "ƒøsT3r!™ " $
        (optInfo ["s", "string"])
            { optName = "STRING"
            , optDoc = "string to be used and eventually repeated"
            }

inArg :: Term FilePath
inArg = 
    value . opt "input.txt" $ 
        (optInfo ["i", "input"])
             { optName = "INPUT"
             , optDoc  = "input file"
             }

outButReallyInArg :: Term FilePath
outButReallyInArg =
    value . opt "input.txt" $
        (optInfo ["o", "output"])
            { optName = "OUTPUT"
            , optDoc = "output file"
            }

outArg :: Term FilePath
outArg = 
    value . opt "output.txt" $ 
        (optInfo ["o", "output"])
             { optName = "OUTPUT"
             , optDoc  = "output file"
             }

generateTerm :: (Term (IO ()), TermInfo)
generateTerm = 
    ( generate 
        <$> sizeArg
        <*> stringArg
        <*> outButReallyInArg
        <*> silentArg
    , defTI 
        { termName = "generate"
        , termDoc = "Generates a new puzzle"
        } )

solveTerm :: (Term (IO ()), TermInfo)
solveTerm = 
    ( solve 
        <$> fileExists inArg
        <*> validPath outArg
        <*> silentArg
    , defTI 
        { termName = "solve"
        , termDoc = "Solves a puzzle"
        } )

checkTerm :: (Term (IO ()), TermInfo)
checkTerm = 
    ( check
        <$> fileExists inArg
        <*> fileExists outArg
        <*> silentArg
    , defTI
        { termName = "check"
        , termDoc = "Checks that a solution to a puzzle is correct"
        } )

baseTerm :: (Term (IO ()), TermInfo)
baseTerm = 
    ( ret $ pure $ helpFail Plain Nothing
    , defTI
        { termName = "foster"
        , version = "1.0.1.1"
        }
    )

interpret :: IO () 
interpret =
    runChoice baseTerm
        [ generateTerm
        , solveTerm
        , checkTerm ]
