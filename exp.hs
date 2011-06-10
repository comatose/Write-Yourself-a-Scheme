data Tree = Term Char | Node Tree Char Tree deriving Show

parseNum ::  String -> Maybe (Tree, String)
parseNum (x:xs)  = Just (Term x, xs)
parseNum _ = Nothing

parseExp ('(':xs) = Just (Node left op right, rest)
                where
                    Just (left, (op:ys)) = parse xs
                    Just (right, (')':rest)) = parse ys
parseExp _ = Nothing

parse xs = case parseExp xs of
                Nothing -> parseNum xs
                x -> x

test = parse "((0+(1+(2*3)))+4)"
