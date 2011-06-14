module Main where

import System
import Text.ParserCombinators.Parsec hiding (spaces)

import Parser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    print . eval . readExpr $ head args

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val
    --
-- trivial tests
test_String = readExpr "\"a\\\" \\r \\n \tabcd\""
test_Atom = readExpr "arntabcd"
test_Number = readExpr "1241"
test_Oct = readExpr "#o10"
test_Char = readExpr "#\\s"
test_Bool = readExpr "#t"
test_Float = readExpr "12.134"
test_List = readExpr "(#\\s #\\space 222 (124 atom 12.32 \"HI1~!\") . '123)"
