module Foster.Solver
    ( solvePuzzle
    ) where

import Data.Map (Map)
import Data.List (find)
import Foster.Data
import qualified Data.Map as M

buildMap :: UnsolvedPuzzle -> Map PieceId Piece
buildMap ps = M.fromList . map buildMapElem $ ps
    where buildMapElem :: Piece -> (PieceId, Piece)
          buildMapElem p = (getId p, p)

getPiecesAlong :: (Piece -> PieceId) -> Map PieceId Piece -> Piece -> [Piece]
getPiecesAlong ex m p
    | ex p == noneId   = [p]
    | otherwise             =
        let (Just np) = M.lookup (ex p) m
        in  p : getPiecesAlong ex m np

solvePuzzle :: UnsolvedPuzzle -> SolvedPuzzle
solvePuzzle ps =
    let m           = buildMap ps
        (Just tl)   = find isTopLeft ps
    in  map (getPiecesAlong getEastId m) (getPiecesAlong getSouthId m tl)
