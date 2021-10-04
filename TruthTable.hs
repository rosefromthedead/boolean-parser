module TruthTable where

import Parser (SyntaxNode (UnaryNode, BinaryNode, NameNode), Unary (Not) ) 
import Data.List (nub)

getNames :: SyntaxNode -> [String]
getNames (UnaryNode Not node) = getNames node
getNames (BinaryNode left op right) = nub $ getNames left ++ getNames right
getNames (NameNode node) = [node]

enumerateNames :: [String] -> [Integer]
enumerateNames x = [0 .. 2 ^ length x - 1]
