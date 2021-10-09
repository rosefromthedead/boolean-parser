module Parser where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)

-- "alice & bob | carol"
-- "x & ((¬y) | z)"

data Unary = Not deriving (Eq, Show)
data Binary = And | Or | Xor | Implies | Equivalent deriving (Eq, Show)
data Op = UnaryOp Unary | BinaryOp Binary deriving (Eq, Show)
data Token = OpToken Op | NameToken String | LeftParen | RightParen deriving (Eq, Show)
data ParensTree = ParensNode [ParensTree] | TokenLeaf Token deriving (Show)
data SyntaxNode = UnaryNode Unary SyntaxNode | BinaryNode SyntaxNode Binary SyntaxNode | NameNode String deriving (Show)

validNameChars = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"
notChars = "!¬~"
andChars = "&*."
orChars = "|+"
xorChars = "^"
impliesChars = "⇒→"
equivalentChars = "≡⇔=⟷"
validChars = validNameChars ++ notChars ++ andChars ++ orChars ++ xorChars ++ impliesChars ++ equivalentChars ++ "() "
presetVals = [("true", True), ("false", False), ("1", True), ("0", False)]

eatWhitespace :: String -> String
eatWhitespace (' ':cs) = eatWhitespace cs
eatWhitespace x = x

eatParen :: String -> Maybe (Token, String)
eatParen ('(':cs) = Just (LeftParen, cs)
eatParen (')':cs) = Just (RightParen, cs)
eatParen x = Nothing

eatName :: String -> String -> Maybe (Token, String)
eatName [] [] = Nothing
eatName left [] = Just (NameToken left, "")
eatName left (c : right)
  | c `elem` validNameChars = eatName (left ++ [c]) right
  | left == "" = Nothing
  | otherwise = Just (NameToken left, c : right)

eatOp :: String -> Maybe (Token, String)
eatOp (c : cs)
  | c `elem` notChars = Just (OpToken $ UnaryOp Not, cs)
  | c `elem` andChars = Just (OpToken $ BinaryOp And, cs)
  | c `elem` orChars = Just (OpToken $ BinaryOp Or, cs)
  | c `elem` xorChars = Just (OpToken $ BinaryOp Xor, cs)
  | c `elem` impliesChars = Just (OpToken $ BinaryOp Implies, cs)
  | c `elem` equivalentChars = Just (OpToken $ BinaryOp Equivalent, cs)
eatOp x = Nothing

tokenise :: String -> [Token]
tokenise "" = []
tokenise x = let (token, cs) = fromJust $ eatParen x <|> eatName "" x <|> eatOp x in token : tokenise (eatWhitespace cs)

wrangleParens :: Bool -> [Token] -> ([ParensTree], [Token])
wrangleParens shouldTerminate (LeftParen : ts) =
    let (inner, right) = wrangleParens False ts in
        let (rightElements, rightRemaining) = wrangleParens shouldTerminate right in
            (ParensNode inner : rightElements, rightRemaining)
wrangleParens True (RightParen : ts) = undefined -- expected EOF, found ')'
wrangleParens False (RightParen : ts) = ([], ts)
wrangleParens shouldTerminate (t : ts) =
    let (elems, remaining) = wrangleParens shouldTerminate ts in (TokenLeaf t : elems, remaining)
wrangleParens True [] = ([], [])
wrangleParens False [] = undefined -- expected ')', found EOF

searchOps :: [ParensTree] -> [ParensTree] -> Binary -> Maybe ([ParensTree], Binary, [ParensTree])
searchOps left (TokenLeaf (OpToken (BinaryOp op)) : es) needle | needle == op = Just (left, op, es)
searchOps left (e:right) needle = searchOps (left ++ [e]) right needle
searchOps left [] _ = Nothing

wrangleOps :: [ParensTree] -> SyntaxNode
wrangleOps [ParensNode inner] = wrangleOps inner
wrangleOps [TokenLeaf (NameToken name)] = NameNode name
wrangleOps (TokenLeaf (OpToken (BinaryOp _)) : _) = undefined
wrangleOps list =
    let search = searchOps [] list in
        let result = search Equivalent <|> search Implies <|> search Xor <|> search Or <|> search And in
            case result of
                Just (left, op, right) -> BinaryNode (wrangleOps left) op (wrangleOps right)
                Nothing -> case list of
                    (TokenLeaf (OpToken (UnaryOp Not)) : right) -> UnaryNode Not $ wrangleOps right
                    _ -> undefined

parse :: String -> SyntaxNode
parse s =
    let ts = tokenise s in
        wrangleOps $ fst $ wrangleParens True ts
