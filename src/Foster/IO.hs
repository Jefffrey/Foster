{-# LANGUAGE OverloadedStrings #-}

module Foster.IO 
    ( askText
    , askSize
    , askOutputPath
    , writeUnsolvedPuzzle
    , readUnsolvedPuzzle
    , writeSolvedPuzzle
    ) where

import Data.Char (isDigit)
import System.IO (FilePath, withFile, hPutStrLn, IOMode(..))
import Foster.Data
import Foster.Utils (splitOn)

askText :: IO String
askText = do
    putStrLn "Incolla il testo del puzzle:"
    getLine

parseSize :: String -> (Int, Int)
parseSize str = 
    let dims = filter (not . null) . splitOn (not . isDigit) $ str
    in if length dims /= 2
        then error "Formato invalido (utilizza: '[larghezza] [altezza]', nel dubbio)"
        else (read $ dims !! 0, read $ dims !! 1)

askSize :: Int -> IO (Int, Int)
askSize len = do
    putStrLn $ "Il testo è lungo " ++ show len ++ " caratteri."
    putStrLn "Definisci una dimensione per il puzzle (e.g: 1 4):"
    putStrLn "(Se definisci una dimensione troppo grande, verranno aggiunti gli spazi necessari alla fine del testo)"
    puzSizeStr <- getLine
    let puzSize = parseSize puzSizeStr
    let nChars = fst puzSize * snd puzSize
    let toAdd = len - nChars
    if toAdd < 0
        then do
            putStrLn $ "Dimensione troppo piccola di " ++ show (abs toAdd) ++ " caratteri."
            askSize len
        else do
            putStrLn $ "Verranno aggiunti " ++ show toAdd ++ " caratteri di spazio." -- really?
            return puzSize

askOutputPath :: IO FilePath
askOutputPath = do
    putStrLn "Definisci il nome del file di output:"
    getLine 

writeUnsolvedPuzzle :: UnsolvedPuzzle -> FilePath -> IO ()
writeUnsolvedPuzzle puz out = do
    withFile out WriteMode $ \fh -> do
        mapM_ (hPutStrLn fh . show) puz
    putStrLn $ "Il puzzle è stato creato in " ++ out

getFileLines :: FilePath -> IO [String]
getFileLines path = readFile path >>= (return . lines)

parsePieces :: [String] -> UnsolvedPuzzle
parsePieces = map parsePiece
    where parsePiece :: String -> Piece
          parsePiece s = 
              let segs = splitOn (== '\t') $ s
              in  Piece { getContent    = head $ segs !! 1
                        , getId         = segs !! 0
                        , getNorthId    = segs !! 2
                        , getEastId     = segs !! 3
                        , getSouthId    = segs !! 4
                        , getWestId     = segs !! 5 }

readUnsolvedPuzzle :: FilePath -> IO UnsolvedPuzzle
readUnsolvedPuzzle inputPath =
    getFileLines inputPath >>= (return . parsePieces)

showPuzzleString :: SolvedPuzzle -> String
showPuzzleString = map getContent . concat

showPuzzleSize :: SolvedPuzzle -> String
showPuzzleSize = (\(w, h) -> concat [show w, " ", show h]) . getPuzzleSize

showPuzzleTable :: SolvedPuzzle -> String
showPuzzleTable = concat . map ((++ "\n") . map getContent)

writeSolvedPuzzle :: SolvedPuzzle -> FilePath -> IO ()
writeSolvedPuzzle puz out = do
    withFile out WriteMode $ \fh -> do
        let hPutStrLn2 fh   = hPutStrLn fh . (++ "\n")
        hPutStrLn2 fh . showPuzzleString    $ puz
        hPutStrLn  fh . showPuzzleTable     $ puz
        hPutStrLn  fh . showPuzzleSize      $ puz
