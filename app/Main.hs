module Main (main) where

import Dfa (fromNfa, matchDfa, minimize)
import Nfa (fromReg, matchNfa)
import Reg (parseReg)
import System.Environment (getArgs)

-- -n pat str : match str with pattern pat using NFA
-- -d pat str : match str with pattern pat using DFA
-- -m pat str : match str with pattern pat using miniDFA
processArgs :: [String] -> IO ()
processArgs args = do
  case args of
    ["-n", pat, str] -> print $ matchNfa (fromReg $ parseReg pat) str
    ["-d", pat, str] -> print $ matchDfa (fromNfa $ fromReg $ parseReg pat) str
    ["-m", pat, str] -> print $ matchDfa (minimize $ fromNfa $ fromReg $ parseReg pat) str
    _ -> putStrLn "Usage: [-m|-n|-d] pattern string"

main :: IO ()
main = do
  args <- getArgs
  processArgs args