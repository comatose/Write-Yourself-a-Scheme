{-# LANGUAGE ExistentialQuantification #-}
module Evaluator where

import Parser
import Control.Monad
import Control.Monad.Error
import Text.ParserCombinators.Parsec hiding (spaces)
import Data.IORef

type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

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

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows = ErrorT . return
--liftThrows (Left err) = throwError err
--liftThrows (Right val) = return val


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

eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval _ val@(Character _) = return val
eval _ val@(Float _) = return val
eval env (Atom id) = getVar env id
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "if", pred, ts, fs]) = 
                     do p <- eval env pred
                        case p of
                             (Bool False) -> eval env fs
                             otherwise -> eval env ts
eval env (List [Atom "set!", Atom var, val]) =
                     eval env val >>= setVar env var
eval env (List [Atom "define", Atom var, val]) =
                     eval env val >>= defineVar env var

eval env (List (Atom f:args)) = 
        mapM (eval env) args >>= (liftThrows . apply f)
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

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
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (&&)),
              ("||", boolBoolBinop (||)),
              ("string=?", strBoolBinop (==)),
              ("string<?", strBoolBinop (<)),
              ("string>?", strBoolBinop (>)),
              ("string<=?", strBoolBinop (<=)),
              ("string>=?", strBoolBinop (>=)),
              ("symbol?", anyUnop isSymbol),
              ("string?", anyUnop isString),
              ("number?", anyUnop isNumber),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

numBoolBinop = boolBinop unpackNum
strBoolBinop = boolBinop unpackStr
boolBoolBinop = boolBinop unpackBool

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpack op [lx, ly] = do x <- unpack lx
                                  y <- unpack ly
                                  return . Bool $ op x y
boolBinop _ _ args = throwError $ NumArgs 2 args

isSymbol, isString, isNumber  :: [LispVal] -> ThrowsError LispVal
isSymbol [(Atom _)] = return $ Bool True
inSymbol _ = return $ Bool False
isString [(String _)] = return $ Bool True
inString _ = return $ Bool False
isNumber [(Number _)] = return $ Bool True
inNumber _ = return $ Bool False

anyUnop op x@[_] = op x
anyUnop _ xs = throwError $ NumArgs 1 xs

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

unpackStr ::  LispVal -> ThrowsError String
unpackStr (String x) = return x
unpackStr (Number x) = return $ show x
unpackStr (Bool x) = return $ show x
unpackStr x = throwError $ TypeMismatch "string" x

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

car :: [LispVal] -> ThrowsError LispVal
car [List (x:_)] = return x
car [DottedList (x:_) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car bad = throwError $ NumArgs 1 bad

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (_:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) y] = return $ DottedList xs y
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr bad = throwError $ NumArgs 1 bad

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x:xs)
cons [x, DottedList xs y] = return $ DottedList (x:xs) y
cons [x, y] = return $ DottedList [x] y
cons bad = throwError $ NumArgs 2 bad

eqv :: [LispVal] -> ThrowsError LispVal
eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) && 
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals v1 v2 (AnyUnpacker up) = 
             do
                v1' <- up v1
                v2' <- up v2
                return $ v1' == v2'
             `catchError` (const $ return False)

unpackers = [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]

equal :: [LispVal] -> ThrowsError LispVal
equal [v1, v2] = do 
        e1 <-  liftM or $ mapM (unpackEquals v1 v2) unpackers
        e2 <- eqv [v1, v2]
        return . Bool $ (e1 || let (Bool x) = e2 in x)
equal bad = throwError $ NumArgs 2 bad

isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Getting an unbound variable: " var)
          (liftIO . readIORef)
          (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do
    env <- liftIO $ readIORef envRef
    maybe (throwError $ UnboundVar "Setting an unbound variable: " var)
          (liftIO . flip writeIORef val)
          (lookup var env)
    return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val = do
--    bound <- liftIO $ isBound envRef var
--    if bound
--       then setVar envRef var val
--       else liftIO $ do
--           newVal <- newIORef val
--           modifyIORef envRef ((var, newVal):)
--           return val
      (setVar envRef var val)
      `catchError`
      (\_ -> liftIO $ do
           newVal <- newIORef val
           modifyIORef envRef ((var, newVal):)
           return val)

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef vals = do
    env <- readIORef envRef
    new <- newBindings
    newIORef $ new ++ env
    where
        newBindings :: IO [(String, IORef LispVal)]
        newBindings = mapM refBind vals
        refBind :: (String, LispVal) -> IO (String, IORef LispVal)
        refBind (var, val) = do
            valRef <- newIORef val
            return (var, valRef)
