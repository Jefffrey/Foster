module Foster.IO 
    ( askText
    , askSize
    , askOutputPath
    , writePuzzle
    ) where

import Data.Char (isDigit)
import System.IO (FilePath, withFile, hPutStrLn, IOMode(..))
import Foster.Utils (splitBy)
import Foster.Data (UnsolvedPuzzle)

askText :: IO String
askText = do
    putStrLn "Incolla il testo del puzzle:"
    getLine

parseSize :: String -> (Int, Int)
parseSize str = 
    let dims = filter (not . null) . splitBy (not . isDigit) $ str
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

writePuzzle :: UnsolvedPuzzle -> FilePath -> IO ()
writePuzzle puz out = do
    withFile out WriteMode $ \fh -> do
        mapM_ (hPutStrLn fh . show) puz
    putStrLn $ "Il puzzle è stato creato in " ++ out
