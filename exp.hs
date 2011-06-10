import Control.Monad.State

type Number = Int
type Operator = Char
data Expr = Term Number | Node Expr Operator Expr deriving Show

parseNum ::  String -> (Maybe Expr, String)
parseNum (x:xs)  = (Just $ Term $ read [x], xs)
parseNum xs = (Nothing, xs)

parseNum' ::  State String (Maybe Expr)
parseNum' = state parseNum

parseOp :: String -> (Maybe Operator, String)
parseOp (x:xs) | x `elem` "+*-/" = (Just x, xs)
               | otherwise = (Nothing, xs)

parseOp' ::  State String (Maybe Operator)
parseOp' = state parseOp

char :: Char -> State String (Maybe Char)
char c = do
            (x:xs) <- get
            if x == c
                then put xs >> (return $ Just x)
                else return Nothing

parseExpr' :: State String (Maybe Expr)
parseExpr' = do
               val <- char '('
               if val == Nothing
                   then return Nothing
                   else do
                       Just left <- parse'
                       Just op <- parseOp'
                       Just right <- parse'
                       char ')'
                       return $ Just $ Node left op right

parse' :: State String (Maybe Expr)
parse' = do
            val <- parseExpr'
            case val of
                Nothing -> parseNum'
                x -> return val

testchar = runState (char '(') "(090)"
testNum = runState parseNum' "1aaa"
testOp = runState parseOp' "+iaa"
testExpr = runState parseExpr' "(1+2)"

test = runState parse' "((0+(1+(2*3)))+4)"
test2 = runState parse' "4 1123"
