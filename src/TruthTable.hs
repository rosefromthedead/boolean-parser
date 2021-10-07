module TruthTable where

import Eval
import Parser
import Data.List (nub, sort, transpose)

getNames :: SyntaxNode -> [String]
getNames (UnaryNode Not node) = getNames node
getNames (BinaryNode left op right) = getNames left ++ getNames right
getNames (NameNode node) | node `elem` map fst presetVals  = []
                         | otherwise = [node]

getUniqueNames :: SyntaxNode -> [String]
getUniqueNames node = nub $ getNames node

enumerateTruthValues :: Integer -> [[Bool]]
enumerateTruthValues 1 = [[False, True]]
enumerateTruthValues x = (replicate (2 ^ (x - 1)) False ++ replicate (2 ^ (x - 1)) True)
    : map (take (2 ^ x) . cycle) (enumerateTruthValues (x - 1))

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
            let inputs = transpose $ enumerateTruthValues $ toInteger $ length names in
                let outputs = evaluateIntList names parsedExpr inputs in
                    foldl (\line name -> line ++ "|" ++ name) "" names ++ "|`" ++ replace "|" "\\|" expr ++ "`|\n" ++
                    "|" ++ concatMap (const "---|") names ++ "---|\n" ++
                    foldl (
                        \line (input, output) -> line ++ foldl (\line val -> line ++ "|" ++ show val) "" input ++ "|" ++ show output ++ "|\n"
                    ) "" (zip inputs outputs)

satisfiable :: SyntaxNode -> Bool
satisfiable expr =
    let names = sort $ getUniqueNames expr in
        let inputs = transpose $ enumerateTruthValues $ toInteger $ length names in
            or $ evaluateIntList names expr inputs
