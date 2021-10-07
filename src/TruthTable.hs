module TruthTable where

import Eval
import Parser
import Data.List (nub, sort)

getNames :: SyntaxNode -> [String]
getNames (UnaryNode Not node) = getNames node
getNames (BinaryNode left op right) = getNames left ++ getNames right
getNames (NameNode node) | node `elem` map fst presetVals  = []
                         | otherwise = [node]

getUniqueNames :: SyntaxNode -> [String]
getUniqueNames node = nub $ getNames node

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

enumerateTruthValues :: Integer -> [[Bool]]
enumerateTruthValues x = map (intToBits x) [0 .. 2 ^ x - 1]

evaluateIntList :: [String] -> SyntaxNode -> [[Bool]] -> [Bool]
evaluateIntList s node = map
      (\ x -> eval ((++) presetVals $ zip s x) node)

replace :: String -> String -> String -> String
replace i o (x : xs) | [x] == i = o ++ replace i o xs
                     | otherwise = x : replace i o xs
replace _ _ "" = ""

formattedMarkdownTable :: String -> String
formattedMarkdownTable expr =
    let parsedExpr = parse expr in
        let names = sort $ getUniqueNames parsedExpr in
            let inputs = enumerateTruthValues $ toInteger $ length names in
                let outputs = evaluateIntList names parsedExpr inputs in
                    foldl (\line name -> line ++ "|" ++ name) "" names ++ "|`" ++ replace "|" "\\|" expr ++ "`|\n" ++
                    "|" ++ concatMap (const "---|") names ++ "---|\n" ++
                    foldl (
                        \line (input, output) -> line ++ foldl (\line val -> line ++ "|" ++ show val) "" input ++ "|" ++ show output ++ "|\n"
                    ) "" (zip inputs outputs)
