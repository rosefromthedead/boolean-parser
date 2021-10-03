-- "alice & bob | carol"
-- "x & ((¬y) | z)"

data Unary = Not deriving (Ord, Eq, Show)
data Binary = And | Or | Xor | Implies | Equivalent deriving (Ord, Eq, Show)
data Op = UnaryOp Unary | BinaryOp Binary deriving (Ord, Eq, Show)
data Token = OpToken Op | NameToken String | LeftParen | RightParen deriving (Eq, Show)
data SyntaxNode = UnaryNode Unary SyntaxNode | BinaryNode SyntaxNode Binary SyntaxNode | NameNode String deriving (Show)

validNameChars = "abcdefghijklmnopqrstuvwxyz"
validChars = validNameChars ++ "&|^()¬⇒≡ "

-- Whitespace brackets names ops

eatWhitespace :: String -> String
eatWhitespace (' ':cs) = eatWhitespace cs
eatWhitespace x = x

eatParen :: String -> (Maybe Token, String)
eatParen ('(':cs) = (Just LeftParen, cs)
eatParen (')':cs) = (Just RightParen, cs)
eatParen x = (Nothing, x)

eatName :: String -> String -> (Maybe Token, String)
eatName soFar (c:cs)
  | c `elem` validNameChars = eatName (soFar ++ [c]) cs
  | soFar == "" = (Nothing, c:cs)
  | otherwise = (Just $ NameToken soFar, c:cs)
eatName [] [] = (Nothing, [])
eatName soFar [] = (Just $ NameToken soFar, "")

eatOp :: String -> (Maybe Token, String)
eatOp ('¬':cs) = (Just $ OpToken $ UnaryOp Not, cs)
eatOp ('&':cs) = (Just $ OpToken $ BinaryOp And, cs)
eatOp ('|':cs) = (Just $ OpToken $ BinaryOp Or, cs)
eatOp ('^':cs) = (Just $ OpToken $ BinaryOp Xor, cs)
eatOp ('⇒':cs) = (Just $ OpToken $ BinaryOp Implies, cs)
eatOp ('≡':cs) = (Just $ OpToken $ BinaryOp Equivalent, cs)
eatOp x = (Nothing, x)

eat :: [Token] -> String -> ([Token], String)
eat tokens x = case eatParen $ eatWhitespace x of
    (Just t, cs) -> (tokens ++ [t], cs)
    (Nothing, _) -> case eatName "" $ eatWhitespace x of
        (Just t, cs) -> (tokens ++ [t], cs)
        (Nothing, _) -> case eatOp $ eatWhitespace x of
            (Just t, cs) -> (tokens ++ [t], cs)
            (Nothing, _) -> undefined

accumulate :: [Token] -> String -> ([Token], String)
accumulate tokens "" = (tokens, "")
accumulate tokens s = uncurry accumulate $ eat tokens s

--"x & & y"
tokenise :: String -> [Token]
tokenise x = fst $ accumulate [] x
