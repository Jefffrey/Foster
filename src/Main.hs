module Main where

import Foster.IO (askText, askSize, askOutputPath, writePuzzle)
import Foster.Generator (generatePuzzle)

main :: IO ()
main = do
    str <- askText
    size <- askSize (length str)
    out <- askOutputPath
    puz <- generatePuzzle size str
    writePuzzle puz out
