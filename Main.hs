module Main where

import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Error
import IO hiding (try)
import Data.IORef

import Parser
import Evaluator

main :: IO ()
main = do
    args <- getArgs
    case length args of
        0 -> runRepl
        1 -> runOne $ head args 
        otherwise -> putStrLn "Program takes only 0 or 1 argument"

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= (eval env)
--evalString env expr = return . extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result
        then return ()
        else action result >> until_ pred prompt action

runOne :: String -> IO ()
runOne expr = nullEnv >>= flip evalAndPrint expr

runRepl :: IO ()
runRepl = do
    env <- nullEnv
    until_ (== "quit") (readPrompt "scheme> ") (evalAndPrint env)

rep ::  String -> IO ()
rep expr = do
    env <- nullEnv
    val <- runIOThrows $ liftM show $ (liftThrows $ readExpr expr) >>= eval env
    print $ show val

--trapError :: (MonadError LispError m) => m String -> m String
trapError ::  (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

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
test_Prims3 = rep "(string=? \"test\"  \"test\")" 
test_if = rep "(if (> 2 3) \"no\" \"yes\")"
