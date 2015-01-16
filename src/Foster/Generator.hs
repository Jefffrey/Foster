module Foster.Generator (generatePuzzle) where
 
import Foster.Data
import Foster.Utils (shuffle)

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

generatePieces :: (Int, Int) -> String -> [Piece]
generatePieces siz str = generatePieces' siz str 0
    where 
        generatePieces' :: (Int, Int) -> String -> Int -> [Piece]
        generatePieces' _ [] _ = []
        generatePieces' sz (c:cs) i = 
            generatePiece sz c i : generatePieces' sz cs (i + 1) 

        generatePiece :: (Int, Int) -> Char -> Int -> Piece
        generatePiece sz c i =
            Piece c (show i) (calcNorthId sz i)
                             (calcEastId sz i)
                             (calcSouthId sz i)
                             (calcWestId sz i)

-- @todo: customize it so that the string is passed as an argument
generatePuzzle :: (Int, Int) -> String -> IO UnsolvedPuzzle
generatePuzzle (w, h) str =
    shuffle . generatePieces (w, h) $ text (w * h)
        where text i = take i . concat . repeat $ str 
