module Main where

import Prelude hiding (id)
import Data.List (intersperse)
import Data.Char (isDigit)
import System.Random
import Control.Applicative
import System.Environment (getArgs)
import System.IO (FilePath, withFile, hPutStrLn, IOMode(..))

splitBy :: (a -> Bool) -> [a] -> [[a]]
splitBy _ [] = []
splitBy c ls = 
    let (l, r) = span (not . c) ls
    in l : (splitBy c . drop 1 $ r)

parseSize :: String -> (Int, Int)
parseSize str = 
    let dims = filter (not . null) . splitBy (not . isDigit) $ str
    in if length dims /= 2
        then error "Formato invalido (utilizza: '[larghezza] [altezza]', nel dubbio)"
        else (read $ dims !! 0, read $ dims !! 1)

askText :: IO String
askText = do
    putStrLn "Incolla il testo del puzzle:"
    getLine

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

generatePuzzle :: String -> (Int, Int) -> FilePath -> IO ()
generatePuzzle str size out = do
    let puz = generateSolvedPuzzle size str
    withFile out WriteMode $ \fh -> do
        (shuffle puz) >>= (mapM_ (hPutStrLn fh . show))
    putStrLn $ "Il puzzle è stato creato in " ++ out

main :: IO ()
main = do
    str <- askText
    size <- askSize (length str)
    out <- askOutputPath
    generatePuzzle str size out
 
type PieceId        = String
type PieceContent   = Char
 
data Piece = Piece { character  :: PieceContent
                   , id         :: PieceId
                   , northId    :: PieceId
                   , eastId     :: PieceId
                   , southId    :: PieceId
                   , westId     :: PieceId
                   } deriving (Eq)
                   
instance Show Piece where
	show (Piece c i ni ei si wi) = concat . intersperse "\t" $ [i, [c], ni, ei, si, wi]
 
noneId :: String
noneId = "VUOTO"
 
type UnsolvedPuzzle = [Piece]
type SolvedPuzzle  	= [[Piece]]
		  
calcNorthId :: (Int, Int) -> Int -> PieceId
calcNorthId (w, _) i =
	let x = i - w
	in if x >= 0
		then show x
		else noneId
	
calcEastId :: (Int, Int) -> Int -> PieceId
calcEastId (w, _) i =
	let x = i + 1
	in if (x `mod` w) /= 0
		then show x
		else noneId
		
calcSouthId :: (Int, Int) -> Int -> PieceId
calcSouthId (w, h) i =
	let x = i + w
	in if (x `div` w) < h
		then show x
		else noneId
		
calcWestId :: (Int, Int) -> Int -> PieceId
calcWestId (w, _) i =
	let x = i - 1
	in if (i `mod` w) /= 0
		then show x
		else noneId

generatePieces :: (Int, Int) -> Int -> String -> [Piece]
generatePieces _ _ [] = []
generatePieces sz i (c:cs) = 
	(Piece c (show i) (calcNorthId sz i)
			  	 	 (calcEastId sz i)
			  		 (calcSouthId sz i)
			  		 (calcWestId sz i)) : generatePieces sz (i + 1) cs
 
generateSolvedPuzzle :: (Int, Int) -> String -> [Piece]
generateSolvedPuzzle (w, h) s
	| w * h /= length s = error "La lunghezza della stringa non corrisponde alla dimensione del puzzle."
	| otherwise			= generatePieces (w, h) 0 $ s

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle lst = do
    (e, rest) <- pickElem <$> getIx
    (e:) <$> shuffle rest
    where
        getIx = getStdRandom $ randomR (1, length lst)
        pickElem n = case splitAt n lst of
            ([], s) -> error "Qualcosa è andato tanto storto"
            (r, s)  -> (last r, init r ++ s)
