module Main where

import System
import Parser

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))

