module Foster.Utils where

splitOn :: (a -> Bool) -> [a] -> [[a]]
splitOn _ [] = []
splitOn c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitOn c . drop 1 $ r)
