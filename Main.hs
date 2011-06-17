module Main where

import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error

import Parser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    rep $ head args

rep ::  String -> IO ()
rep expr = 
    let val = extractValue $ trapError (readExpr expr >>= eval)
    in print $ show val

trapError ::  ThrowsError LispVal -> ThrowsError LispVal
trapError action = catchError action (return . String . show)

extractValue ::  ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val
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
test_Prims = rep "(- (+ 4 6 3) 3 5 2)"
test_Prims2 = rep "(string? \"Hello\")"
test_Error = rep "(+ 2 \"two\")"
