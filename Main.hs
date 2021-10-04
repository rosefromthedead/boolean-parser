module Main where

import Eval
import Parser
import TruthTable
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    putStrLn $ formattedMarkdownTable $ head args
