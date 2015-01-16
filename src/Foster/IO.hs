module Foster.IO 
    ( writeUnsolvedPuzzle
    , readUnsolvedPuzzle
    , writeSolvedPuzzle
    , showSolvedPuzzle
    ) where

import System.IO (withFile, hPutStr, hPrint, IOMode(..))
import Control.Monad (liftM)
import Foster.Data
import Foster.Utils (getFileLines)
import Foster.Parser (parsePieces)

writeUnsolvedPuzzle :: FilePath -> UnsolvedPuzzle -> IO ()
writeUnsolvedPuzzle out puz =
    withFile out WriteMode $ \fh ->
        mapM_ (hPrint fh) puz

readUnsolvedPuzzle :: FilePath -> IO UnsolvedPuzzle
readUnsolvedPuzzle inputPath =
    liftM parsePieces (getFileLines inputPath)

showPuzzleString :: SolvedPuzzle -> String
showPuzzleString = map getContent . concat

showPuzzleSize :: SolvedPuzzle -> String
showPuzzleSize = (\(w, h) -> concat [show h, " ", show w]) . getPuzzleSize

showPuzzleTable :: SolvedPuzzle -> String
showPuzzleTable = concatMap ((++ "\n") . map getContent)

showSolvedPuzzle :: SolvedPuzzle -> String
showSolvedPuzzle puz =
    concat
        [ showPuzzleString puz, "\n\n"
        , showPuzzleTable puz, "\n"
        , showPuzzleSize puz ]

writeSolvedPuzzle :: FilePath -> SolvedPuzzle -> IO ()
writeSolvedPuzzle out puz =
    withFile out WriteMode $ \fh ->
        hPutStr fh (showSolvedPuzzle puz)
