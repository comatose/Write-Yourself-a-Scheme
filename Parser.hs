module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Test.QuickCheck
import Numeric

data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char 
    | Float Double
    deriving (Eq)

instance Show LispVal where
    show = showVal

parseExpr :: Parser LispVal
parseExpr = parseString
        <|> parseBool 
        <|> parseNumber 
        <|> parseAtom 
        <|> parseChar
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

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
parseNumber =  parseFloat <|> parseDec <|> parseDec' <|> parseOct <|> parseHex

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

parseChar :: Parser LispVal
parseChar = do
    try $ string "#\\"
    namedParse <|> rawParse
    where 
     namedParse = 
         (try $ string "space" >> (return $ Character ' '))
         <|> (try $ string "newline" >> (return $ Character '\n'))
     rawParse = 
         do { c <- letter; notFollowedBy alphaNum; return $ Character c }

parseFloat :: Parser LispVal
parseFloat = try $ do
    x <- many1 digit
    char '.'
    y <- many digit
    return $ Float . fst . head $ readFloat (x ++ "." ++ y)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces
--parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List xs) = "(" ++ unwordList xs ++ ")"
showVal (DottedList x y) = "(" ++ unwordList x ++ " . " ++ showVal y ++ ")"
showVal (Character c) = show c
showVal (Float f) = show f

unwordList :: [LispVal] -> String
unwordList = unwords . map showVal

-- trivial tests
test_String = readExpr "\"a\\\" \\r \\n \tabcd\""
test_Atom = readExpr "arntabcd"
test_Number = readExpr "1241"
test_Oct = readExpr "#o10"
test_Char = readExpr "#\\s"
test_Bool = readExpr "#t"
test_Float = readExpr "12.134"
test_List = readExpr "(#\\s #\\space 222 (124 atom 12.32 \"HI1~!\") . '123)"
