module Foster.Solver
    ( solve
    , solvePuzzle
    ) where

import Data.Map (Map)
import Data.List (find)
import Foster.Data
import Foster.Utils
import Control.Monad (unless)
import Foster.IO (writeSolvedPuzzle, readUnsolvedPuzzle)
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

solvePuzzle :: Bool -> UnsolvedPuzzle -> IO SolvedPuzzle
solvePuzzle sil ps =
    let m           = buildMap ps
        (Just tl)   = find isTopLeft ps
        firstCol    = getPiecesAlong getSouthId m tl
        rs          = length firstCol
    in  do
        sp <- mapM 
            (\(i, p) -> do
                unless sil $ putPercOver (i, rs) "Solving"
                return $ getPiecesAlong getEastId m p) 
            (zip [1..rs] firstCol)
        putStrLn "" >> flush
        return sp

solve :: FilePath -> FilePath -> Bool -> IO ()
solve inp out sil = do
    unless sil $ putPercOver (0, 1) "Reading" >> flush
    up <- readUnsolvedPuzzle inp
    unless sil $ putPercOver (1, 1) "Reading" >> putStrLn ""
    sp <- solvePuzzle sil up
    unless sil $ putPercOver (0, 1) "Saving" >> flush
    writeSolvedPuzzle out sp
    unless sil $ do
        putPercOver (1, 1) "Saving" >> putStrLn ""
        putStrLn $ "Solved → " ++ out
