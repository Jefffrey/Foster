module Foster.Parser where

import Foster.Data
import Foster.Utils (splitOn)

parsePiece :: String -> Piece
parsePiece s = 
    let segs = splitOn (== '\t') s
    in  Piece { getContent    = head $ segs !! 1
              , getId         = head segs
              , getNorthId    = segs !! 2
              , getEastId     = segs !! 3
              , getSouthId    = segs !! 4
              , getWestId     = segs !! 5 }

parsePieces :: [String] -> UnsolvedPuzzle
parsePieces = map parsePiece
