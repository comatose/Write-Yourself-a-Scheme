module Main where

import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad

data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool deriving Show

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val
        
spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                 char '"'
                 x <- many $ escape <|> (noneOf "\"")
                 --x <- many $ (char '\\' >> char '\"') <|> (noneOf "\"")
                 char '"'
                 return $ String x

escape :: Parser Char
escape = do
                 char '\\'
                 x <- oneOf "nrt\"\\"
                 case x of
                    'n' -> return '\n'
                    'r' -> return '\r'
                    't' -> return '\t'
                    '\"' -> return '\"'
                    '\\' -> return '\\'

parseAtom :: Parser LispVal
parseAtom = do
               x <- letter <|> symbol
               xs <- many (letter <|> symbol <|> digit)
               let val = x:xs
               return $ case val of
                   "#t" -> Bool True
                   "#f" -> Bool False
                   _ -> Atom val

parseNumber :: Parser LispVal
--parseNumber = (many1 digit) >>= return . Number . read
--parseNumber = liftM (Number . read) $ many1 digit
parseNumber = do val <- many1 digit
                 return . Number $ read val

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseAtom <|> parseNumber
