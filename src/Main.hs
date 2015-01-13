module Main where

import Foster.Interpreter (interpret)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= interpret 
