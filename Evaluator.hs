module Evaluator where

import Parser
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)

instance Show LispVal where
    show = showVal

data LispError = NumArgs Integer [LispVal]
                | TypeMismatch String LispVal
                | Parser ParseError
                | BadSpecialForm String LispVal
                | NotFunction String String
                | UnboundVar String String
                | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected 
                     ++ " args; found values " ++ unwordList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                     ++ ", found " ++ show found
--showError (Parser parseErr) = "Parse error at " ++ show parseErr 

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An Error has occured"
    strMsg = Default

type ThrowsError = Either LispError

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

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return $ val
eval val@(Number _) = return $ val
eval val@(Bool _) = return $ val
eval val@(Character _) = return $ val
eval val@(Float _) = return $ val
eval (List [Atom "quote", val]) = return $ val

eval (List (Atom f:args)) = apply f =<< mapM eval args 
--eval (List (Atom f:args)) = do
--    xs <- evalArgs args
--    apply f xs
--    where evalArgs :: [LispVal] -> ThrowsError [LispVal]
--          evalArgs (x:xs) = do v <- eval x
--                               vs <- evalArgs xs
--                               return (v:vs)

eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply ::  String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function args" f) ($ args) op
    where op = lookup f primitives

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("symbol?", isSymbol),
              ("string?", isString),
              ("number?", isNumber) ]

type UnaryOp = [LispVal] -> ThrowsError LispVal
isSymbol, isString, isNumber  :: UnaryOp

isSymbol [(Atom _)] = return $ Bool True
inSymbol _ = return $ Bool False
isString [(String _)] = return $ Bool True
inString _ = return $ Bool False
isNumber [(Number _)] = return $ Bool True
inNumber _ = return $ Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop _ x@[_] = throwError $ NumArgs 2 x
numericBinop op xs = do args <- mapM unpackNum xs
                        return . Number $ foldl1 op args

unpackNum ::  LispVal -> ThrowsError Integer
unpackNum (Number x) = return x
unpackNum (String x) = let n = reads x in
                           if null n
                              then throwError $ TypeMismatch "number" (String x)
                              else return . fst $ head n

unpackNum (List [x]) = unpackNum x
unpackNum x = throwError $ TypeMismatch "number" x
