module Foster.IO 
    ( writeUnsolvedPuzzle
    , readUnsolvedPuzzle
    , writeSolvedPuzzle
    ) where

import System.IO (withFile, hPutStrLn, hPrint, IOMode(..))
import Control.Monad (liftM)
import Foster.Data
import Foster.Utils (getFileLines)
import Foster.Parser (parsePieces)

writeUnsolvedPuzzle :: FilePath -> UnsolvedPuzzle -> IO ()
writeUnsolvedPuzzle out puz = do
    withFile out WriteMode $ \fh ->
        mapM_ (hPrint fh) puz

readUnsolvedPuzzle :: FilePath -> IO UnsolvedPuzzle
readUnsolvedPuzzle inputPath =
    liftM parsePieces (getFileLines inputPath)

showPuzzleString :: SolvedPuzzle -> String
showPuzzleString = map getContent . concat

showPuzzleSize :: SolvedPuzzle -> String
showPuzzleSize = (\(w, h) -> concat [show w, " ", show h]) . getPuzzleSize

showPuzzleTable :: SolvedPuzzle -> String
showPuzzleTable = concatMap ((++ "\n") . map getContent)

writeSolvedPuzzle :: FilePath -> SolvedPuzzle -> IO ()
writeSolvedPuzzle out puz =
    withFile out WriteMode $ \fh -> do
        let hPutStrLn2 fhi = hPutStrLn fhi . (++ "\n")
        hPutStrLn2 fh . showPuzzleString    $ puz
        hPutStrLn  fh . showPuzzleTable     $ puz
        hPutStrLn  fh . showPuzzleSize      $ puz
