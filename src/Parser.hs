module Parser where

import Control.Applicative ((<|>))
import Data.Maybe (fromJust)
import Data.Bifunctor (first)

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

eat :: String -> Maybe (Token, String)
eat (' ' : cs) = eat cs
eat ('(' : cs) = Just (LeftParen, cs)
eat (')' : cs) = Just (RightParen, cs)
eat (c : cs) | c `elem` validNameChars = Just $ first NameToken $ span (`elem` validNameChars) (c : cs)
eat (c : cs)
  | c `elem` notChars = Just (OpToken $ UnaryOp Not, cs)
  | c `elem` andChars = Just (OpToken $ BinaryOp And, cs)
  | c `elem` orChars = Just (OpToken $ BinaryOp Or, cs)
  | c `elem` xorChars = Just (OpToken $ BinaryOp Xor, cs)
  | c `elem` impliesChars = Just (OpToken $ BinaryOp Implies, cs)
  | c `elem` equivalentChars = Just (OpToken $ BinaryOp Equivalent, cs)
eat x = Nothing

tokenise :: String -> [Token]
tokenise "" = []
tokenise x = let (token, cs) = fromJust $ eat x in token : tokenise cs

wrangleParens :: [Token] -> ([ParensTree], [Token])
wrangleParens (LeftParen : ts) =
    let (inner, right) = wrangleParens ts in
        let (rightElements, rightRemaining) = wrangleParens right in
            (ParensNode inner : rightElements, rightRemaining)
wrangleParens (RightParen : ts) = ([], ts)
wrangleParens (t : ts) = first (TokenLeaf t :) $ wrangleParens ts
wrangleParens [] = undefined

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
parse s = wrangleOps $ fst $ wrangleParens (LeftParen : tokenise s ++ [RightParen])
