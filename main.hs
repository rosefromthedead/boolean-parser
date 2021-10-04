import Parser
import TruthTable
import Data.List (find)
import Data.Maybe (fromJust)
import Data.Bits (xor)

function :: String -> [String]
function x =
    let tree = parse x in
        getNames tree

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
