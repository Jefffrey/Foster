module Foster.Data where

import Data.List (intercalate)

type PieceId        = String
type PieceContent   = Char
 
data Piece = Piece { getContent :: PieceContent
                   , getId      :: PieceId
                   , getNorthId :: PieceId
                   , getEastId  :: PieceId
                   , getSouthId :: PieceId
                   , getWestId  :: PieceId
                   } deriving (Eq)
                   
instance Show Piece where
	show (Piece c i ni ei si wi) = intercalate "\t" [i, [c], ni, ei, si, wi]
 
type UnsolvedPuzzle = [Piece]
type SolvedPuzzle  	= [[Piece]]
type Size           = (Int, Int)

noneId :: PieceId
noneId = "VUOTO"

isTopLeft :: Piece -> Bool
isTopLeft p = getNorthId p == noneId && getWestId p == noneId

getPuzzleSize :: SolvedPuzzle -> (Int, Int)
getPuzzleSize sp = (w, h)
    where w = (length . head) sp
          h = length sp
