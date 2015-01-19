{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Foster.Interpreter
    ( interpret
    ) where

import Foster.Generator (generate)
import Foster.Solver (solve)
import Foster.Checker (check)
import Control.Applicative
import System.Console.CmdTheLine
import Foster.Data (Size)

instance ArgVal Size where
    converter = pair ','

instance ArgVal (Maybe Size) where
    converter = just

silentArg :: Term Bool
silentArg = value . flag $ optInfo ["silent", "s"]

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
    value . opt "ƒøsT3r! " $
        (optInfo ["t", "text"])
            { optName = "TEXT"
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
        , version = "1.1.2"
        }
    )

interpret :: IO () 
interpret =
    runChoice baseTerm
        [ generateTerm
        , solveTerm
        , checkTerm ]
