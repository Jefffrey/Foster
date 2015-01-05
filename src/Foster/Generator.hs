module Foster.Generator (generatePuzzle) where
 
import Foster.Data
import System.Random
import Control.Applicative

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
 
generatePuzzle :: (Int, Int) -> String -> IO UnsolvedPuzzle
generatePuzzle (w, h) s
	| w * h /= length s = error "La lunghezza della stringa non corrisponde alla dimensione del puzzle."
	| otherwise			= shuffle . generatePieces (w, h) 0 $ s
