-- "alice & bob | carol"
-- "x & ((¬y) | z)"

data NullaryOp = TrueOp | FalseOp deriving (Ord, Eq)
data UnaryOp = Not deriving (Ord, Eq)
data BinaryOp = And | Or | Xor | Implies | Equivalent deriving (Ord, Eq)
data Op = Nullary NullaryOp | Unary UnaryOp | Binary BinaryOp deriving (Ord, Eq)
data Token = OpToken Op | NameToken String | LeftParen | RightParen
data SyntaxNode = NullaryNode NullaryOp | UnaryNode UnaryOp SyntaxNode | BinaryNode BinaryOp SyntaxNode SyntaxNode | Name String

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

eatName :: String -> String -> (Maybe String, String)
eatName soFar (c:cs) =
    if contains validNameChars c
        then eatName (soFar ++ [c]) cs
        else (if soFar == "" then (Nothing, (c:cs)) else (Just soFar, (c:cs)))
eatName [] [] = (Nothing, [])
eatName soFar [] = (Just soFar, "")

eatOp :: String -> (Token, String)
eatOp ('¬':cs) = (OpToken $ Unary Not, cs)
eatOp ('&':cs) = (OpToken $ Binary And, cs)
eatOp ('|':cs) = (OpToken $ Binary Or, cs)
eatOp ('^':cs) = (OpToken $ Binary Xor, cs)
eatOp ('⇒':cs) = (OpToken $ Binary Implies, cs)
eatOp ('≡':cs) = (OpToken $ Binary Equivalent, cs)

--"x & & y"
--tokenise :: Int -> String -> [Op]
--tokenise 0 "" = Nothing
--tokenise _ "" = undefined
--tokenise 
