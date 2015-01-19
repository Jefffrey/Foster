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
solvePuzzle sil ps = do
    unless sil $ putPercOver (0, 4) "Preparing" >> flush
    let m = buildMap ps
    m `seq` unless sil $ putPercOver (1, 4) "Preparing" >> flush
    let (Just tl) = find isTopLeft ps
    tl `seq` unless sil $ putPercOver (2, 4) "Preparing" >> flush
    let firstCol = getPiecesAlong getSouthId m tl
    firstCol `seq` unless sil $ putPercOver (3, 4) "Preparing" >> flush
    let rs = length firstCol
    rs `seq` unless sil $ putPercOver (4, 4) "Preparing" >> putStrLn ""
    sp <- mapM 
        (\(i, p) -> do
            unless sil $ putPercOver (i, rs) "Solving"
            return $ getPiecesAlong getEastId m p) 
        (zip [1..rs] firstCol)
    unless sil $ putStrLn "" >> flush
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
