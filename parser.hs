-- "alice & bob | carol"
-- "x & ((¬y) | z)"

data NullaryOp = TrueOp | FalseOp deriving (Ord, Eq, Show)
data UnaryOp = Not deriving (Ord, Eq, Show)
data BinaryOp = And | Or | Xor | Implies | Equivalent deriving (Ord, Eq, Show)
data Op = Nullary NullaryOp | Unary UnaryOp | Binary BinaryOp deriving (Ord, Eq, Show)
data Token = OpToken Op | NameToken String | LeftParen | RightParen deriving (Eq, Show)
data SyntaxNode = NullaryNode NullaryOp | UnaryNode UnaryOp SyntaxNode | BinaryNode BinaryOp SyntaxNode SyntaxNode | Name String deriving (Show)

cumulativeCheck :: (Eq a) => a -> a -> Bool -> Bool
cumulativeCheck needle element soFar = soFar || needle == element

contains :: (Eq a) => [a] -> a -> Bool
contains haystack needle = foldr (cumulativeCheck needle) False haystack

validChars = "abcdefghijklmnopqrstuvwxyz&|^()¬⇒≡ "
validNameChars = "abcdefghijklmnopqrstuvwxyz"

usesValidSymbols :: String -> Bool
usesValidSymbols [] = True
usesValidSymbols (c:cs) =
    contains validChars c && usesValidSymbols cs

-- Whitespace brackets names ops

eatWhitespace :: String -> String
eatWhitespace (' ':cs) = eatWhitespace cs
eatWhitespace x = x

eatParen :: String -> (Maybe Token, String)
eatParen ('(':cs) = (Just LeftParen, cs)
eatParen (')':cs) = (Just RightParen, cs)
eatParen x = (Nothing, x)

eatName :: String -> String -> (Maybe Token, String)
eatName soFar (c:cs) =
    if contains validNameChars c
        then eatName (soFar ++ [c]) cs
        else (if soFar == "" then (Nothing, (c:cs)) else (Just $ NameToken soFar, (c:cs)))
eatName [] [] = (Nothing, [])
eatName soFar [] = (Just $ NameToken soFar, "")

eatOp :: String -> (Maybe Token, String)
eatOp ('¬':cs) = (Just $ OpToken $ Unary Not, cs)
eatOp ('&':cs) = (Just $ OpToken $ Binary And, cs)
eatOp ('|':cs) = (Just $ OpToken $ Binary Or, cs)
eatOp ('^':cs) = (Just $ OpToken $ Binary Xor, cs)
eatOp ('⇒':cs) = (Just $ OpToken $ Binary Implies, cs)
eatOp ('≡':cs) = (Just $ OpToken $ Binary Equivalent, cs)
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
