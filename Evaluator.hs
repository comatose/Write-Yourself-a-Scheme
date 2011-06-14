module Evaluator where

import Parser

instance Show LispVal where
    show = showVal

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

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Character _) = val
eval val@(Float _) = val
eval (List [Atom "quote", val]) = val

eval (List (Atom f:args)) = apply f (map eval args)

apply f args = maybe (Bool False) ($ args) op
    where op = lookup f primitives

primitives = undefined
