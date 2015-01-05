module Foster.Parser where

import Foster.Data
import Foster.Utils (splitOn)
import Data.Char (isDigit)

parsePiece :: String -> Piece
parsePiece s = 
    let segs = splitOn (== '\t') $ s
    in  Piece { getContent    = head $ segs !! 1
              , getId         = segs !! 0
              , getNorthId    = segs !! 2
              , getEastId     = segs !! 3
              , getSouthId    = segs !! 4
              , getWestId     = segs !! 5 }

parsePieces :: [String] -> UnsolvedPuzzle
parsePieces = map parsePiece

parseSize :: String -> Maybe (Int, Int)
parseSize str = 
    let dims = filter (not . null) . splitOn (not . isDigit) $ str
    in if length dims /= 2
        then Nothing
        else Just (read $ dims !! 0, read $ dims !! 1)
