module Foster.Utils where

import Control.Monad
import System.IO (stdout, hFlush)

getFileLines :: FilePath -> IO [String]
getFileLines path = liftM lines (readFile path)

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn c ls = 
    let (l, r) = break c ls
    in l : (splitOn c . drop 1 $ r)

putStrOver :: String -> IO ()
putStrOver str = putStr $ "\r" ++ str ++ "\o33[K"

putPercOver :: (Int, Int) -> String -> IO ()
putPercOver (a, b) str = do
    let perc = (a * 100) `div` b
    putStrOver . concat $
        [ "[", show perc, "%] "
        , str, "..."
        ]

flush :: IO ()
flush = hFlush stdout
