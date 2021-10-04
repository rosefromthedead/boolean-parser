module TruthTable where

import Eval
import Parser
import Data.List (nub)

getNames :: SyntaxNode -> [String]
getNames (UnaryNode Not node) = getNames node
getNames (BinaryNode left op right) = nub $ getNames left ++ getNames right
getNames (NameNode node) | node `elem` map fst presetVals  = []
                         | otherwise = [node]

enumerateNames :: [String] -> [[Bool]]
enumerateNames x = map (intToBits (toInteger $ length x)) [0 .. 2 ^ length x - 1]

-- https://stackoverflow.com/a/28522641
intToBits :: Integer -> Integer -> [Bool]
intToBits  0 x = []
intToBits sz 0 = [False | i <- [1..sz]]
intToBits sz x =  if k == 0
    then False : intToBits n x
    else True  : intToBits n (x - k*m)
    where n = sz - 1
          m = 2^n
          k = x `div` m

evaluateIntList :: [String] -> SyntaxNode -> [[Bool]] -> [Bool]
evaluateIntList s node = map
      (\ x -> eval ((++) presetVals $ zip s x) node)

function :: String -> [Bool]
function x =
    let tree = parse x in
        let names = getNames tree in
            let evals = enumerateNames names in
                evaluateIntList names tree evals

formattedMarkdownTable :: String -> String
formattedMarkdownTable expr =
    let parsedExpr = parse expr in
        let names = getNames parsedExpr in
            let inputs = enumerateNames names in
                let outputs = evaluateIntList names parsedExpr inputs in
                    foldl (\line name -> line ++ "|" ++ name) "" names ++ "|`" ++ expr ++ "`|\n" ++
                    "|" ++ concatMap (const "---|") names ++ "---|\n" ++
                    foldl (
                        \line (input, output) -> line ++ foldl (\line val -> line ++ "|" ++ show val) "" input ++ "|" ++ show output ++ "|\n"
                    ) "" (zip inputs outputs)
