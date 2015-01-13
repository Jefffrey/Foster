module Foster.Utils where

import Control.Monad (liftM)

getFileLines :: FilePath -> IO [String]
getFileLines path = liftM lines (readFile path)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn c ls = 
    let (l, r) = break c ls
    in l : (splitOn c . drop 1 $ r)
