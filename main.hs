import Parser
import TruthTable
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Bits (xor)

-- https://stackoverflow.com/a/28522641
intToBits :: Integer -> Integer -> [Bool]
intToBits  0 x = []
intToBits sz 0 = [False | i <- [1..sz]]
intToBits sz x =  if k == 0 
    then False : (intToBits n x) 
    else True  : (intToBits n (x - k*m))
    where n = sz - 1
          m = 2^n
          k = x `div` m

combineLists :: [(String, Bool)] -> [String] -> [Bool] -> [(String, Bool)]
combineLists soFar (x : xs) (i : is) = combineLists (soFar ++ [(x, i)]) xs is
combineLists soFar [] [] = soFar

evaluateIntList :: [String] -> SyntaxNode -> [Integer] -> [Bool]
evaluateIntList s node (x : xs) = eval ((++) [("t", True), ("f", False)] $ combineLists [] s $ intToBits (toInteger $ length s) x) node : evaluateIntList s node xs
evaluateIntList s node [] = []

function :: String -> [Bool]
function x =
    let tree = parse x in
        let names = getNames tree in
            let evals = enumerateNames names in
                evaluateIntList names tree evals

impl :: Bool -> Bool -> Bool
impl True False = False
impl _ _ = True

eval :: [(String, Bool)] -> SyntaxNode -> Bool
eval l (UnaryNode Not node) = eval l node
eval l (BinaryNode left And right) = eval l left && eval l right
eval l (BinaryNode left Or right) = eval l left || eval l right
eval l (BinaryNode left Xor right) = eval l left `xor` eval l right
eval l (BinaryNode left Implies right) = eval l left `impl` eval l right
eval l (BinaryNode left Equivalent right) = eval l left == eval l right
eval l (NameNode name) = snd $ fromJust $ find (\(key, value) -> key == name) l
