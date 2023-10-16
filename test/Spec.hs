module Main (main) where

import Nfa (matchNfa, fromReg)
import Dfa (matchDfa, fromNfa, minimize)
import Reg (Reg (..), parseReg)
import Test.Hspec
import Debug.Trace


main :: IO ()
main = hspecTest


type Pattern = String
-- type Pattern = Reg Char

type TestCase = (Pattern, String, Bool)
test_cases :: [TestCase]
test_cases = [
  ("ab", "ab", True),
  ("a*", "aaaaa", True),
  ("a|b", "b", True),
  ("a(b*)", "abbbc", False)
  ]

hspecTest :: IO ()
hspecTest = hspec $ do
  testEngine "NFA" matchNfa Nfa.fromReg test_cases
  testEngine "DFA" matchDfa (Dfa.fromNfa . Nfa.fromReg) test_cases
  testEngine "miniDFA" matchDfa (Dfa.minimize . Dfa.fromNfa . Nfa.fromReg) test_cases

testEngine :: String -> (a -> String -> Bool) -> (Reg Char -> a) -> [TestCase] -> SpecWith ()
testEngine name match from cases =
  describe (name ++ " Test") $
    mapM_ (test name match from) cases

test :: String -> (a -> String -> Bool) -> (Reg Char -> a) -> TestCase -> SpecWith ()
test name match from (pat, str, ans) =
  it (name ++ " match " ++ show pat) $
    match (from $ parseReg pat) str `shouldBe` ans