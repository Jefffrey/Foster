module Foster.Generator (generate) where
 
import Foster.Data
import Foster.Utils
import qualified Data.ByteString.Char8 as B
import Data.ByteString.Char8 (ByteString)
import Foster.IO (writeUnsolvedPuzzle)
import Control.Monad
import System.Random
import Data.Array.IO hiding (newArray)

calcNorthId :: Size -> Int -> PieceId
calcNorthId (w, _) i =
	let x = i - w
	in if x >= 0
		then show x
		else noneId
	
calcEastId :: Size -> Int -> PieceId
calcEastId (w, _) i =
	let x = i + 1
	in if (x `mod` w) /= 0
		then show x
		else noneId
		
calcSouthId :: Size -> Int -> PieceId
calcSouthId (w, h) i =
	let x = i + w
	in if (x `div` w) < h
		then show x
		else noneId
		
calcWestId :: Size -> Int -> PieceId
calcWestId (w, _) i =
	let x = i - 1
	in if (i `mod` w) /= 0
		then show x
		else noneId

generatePiece :: Size -> ByteString -> Int -> Piece
generatePiece sz s i =
    let c = B.index s (i `mod` B.length s)
    in  Piece
            c 
            (show i)
            (calcNorthId sz i)
            (calcEastId sz i)
            (calcSouthId sz i)
            (calcWestId sz i)

generatePieces :: Size -> Bool -> ByteString -> IO [Piece]
generatePieces siz@(w, h) sil str = do
    putStrLn (B.unpack str)
    let tot = w * h
    ps <- mapM 
        (\i -> do
            unless sil $ putPercOver (i + 1, tot) "Generating"
            return $ generatePiece siz str i)
        [0..(tot - 1)]
    unless sil $ putStrLn "" >> flush
    return ps

-- @todo: 
--      we can probably get rid of this
--      with a total injective mapping function
--      to generate pieces already "shuffled"
shuffle :: Bool -> [a] -> IO [a]
shuffle sil xs = do
        ar <- newArray n xs
        es <- forM [1..n] $ \i -> do
            unless sil $ putPercOver (i, n) "Shuffling"
            j <- randomRIO (i,n)
            vi <- readArray ar i
            vj <- readArray ar j
            writeArray ar j vi
            return vj
        unless sil $ putStrLn "" >> flush
        return es
  where
    n = length xs
    newArray :: Int -> [a] -> IO (IOArray Int a)
    newArray ni = newListArray (1, ni)

generatePuzzle :: Size -> String -> Bool -> IO UnsolvedPuzzle
generatePuzzle (w, h) str sil = do
    ps <- generatePieces (w, h) sil (B.pack str)
    shuffle sil ps

generate :: Size -> String -> FilePath -> Bool -> IO ()
generate (h, w) str out sil = do
    puz <- generatePuzzle (w, h) str sil
    unless sil $ putPercOver (0, 1) "Saving" >> flush
    writeUnsolvedPuzzle out puz
    unless sil $ do
        putPercOver (1, 1) "Saving" >> putStr "\n"
        putStrLn $ "Generated → " ++ out
