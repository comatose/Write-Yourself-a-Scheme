module Parser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Test.QuickCheck
import Numeric
import Data.IORef
import Control.Monad.Error
import IO(Handle)

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

type ThrowsError = Either LispError

type IOThrowsError = ErrorT LispError IO

type Env = IORef [(String, IORef LispVal)]

data LispVal = Atom String 
    | List [LispVal]
    | DottedList [LispVal] LispVal
    | Number Integer
    | String String
    | Bool Bool
    | Character Char 
    | Float Double
    | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
    | Func {params :: [String], vararg :: (Maybe String),
        body :: [LispVal], closure :: Env}
    | IOFunc ([LispVal] -> IOThrowsError LispVal)
    | Port Handle

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

