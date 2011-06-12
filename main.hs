module Main where

import System
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Test.QuickCheck
import Numeric

data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool deriving (Eq, Show)

main :: IO ()
main = do
    args <- getArgs
    putStrLn (readExpr (head args))

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right val -> "Found value " ++ show val
        
spaces :: Parser ()
spaces = skipMany1 space

parseString :: Parser LispVal
parseString = do
                 char '"'
                 x <- many $ escapedChars <|> (noneOf "\"\\")
                 --x <- many $ (char '\\' >> char '\"') <|> (noneOf "\"")
                 char '"'
                 return $ String x

escapedChars :: Parser Char
escapedChars = do
                 char '\\'
                 x <- oneOf "nrt\"\\"
                 case x of
                    'n' -> return '\n'
                    'r' -> return '\r'
                    't' -> return '\t'
                    '\"' -> return '\"'
                    '\\' -> return '\\'
                    _ -> fail "escape error!"

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
parseNumber =  parseDec <|> parseDec' <|> parseOct <|> parseHex

parseBool :: Parser LispVal
parseBool = parseTrue <|> parseFalse

parseTrue = do try $ string "#f"
               return $ Bool False

parseFalse = do try $ string "#t"
                return $ Bool True

parseDec:: Parser LispVal
parseDec = do val <- many1 digit
              return . Number $ read val

parseDec' :: Parser LispVal
parseDec'  = do try $ string "#d"
                val <- many1 digit
                return . Number $ read val

parseOct :: Parser LispVal
parseOct  = do try $ string "#o"
               val <- many1 octDigit
               return . Number . fst . head $ readOct val

parseHex :: Parser LispVal
parseHex  = do try $ string "#x"
               val <- many1 hexDigit
               return . Number . fst . head $ readHex val

parseExpr :: Parser LispVal
parseExpr = parseString <|> parseBool <|> parseNumber <|> parseAtom 

test_String = readExpr "\"a\\\" \\r \\n \tabcd\""
test_Atom = readExpr "arntabcd"
test_Number = readExpr "1241"
test_Oct = readExpr "#o10"
test_Bool = readExpr "#t"
