module Main (main) where

import Data.Set
import Nfa (Nfa (..), Trans (..), fromReg, matchNfa, runNfa)
import Reg (Reg (..))
import Test.Hspec

re = Alt (Con (Literal 'a') (Literal 'b')) (Star (Literal 'c'))

main :: IO ()
main = hspecTest

hspecTest :: IO ()
hspecTest = hspec $ do
  describe "Reg" $ do
    it "can show regex expressions" $ do
      show re `shouldBe` "(('a''b')|'c'*)"
    -- it "convert regex to NFA" $ do
    --     fromReg re `shouldBe` ...
    it "nfa match string" $ do
      matchNfa (fromReg re) "ab" `shouldBe` True
    it "nfa match string2" $ do
      matchNfa (fromReg re) "ccccc" `shouldBe` True
    it "nfa match string3" $ do
      matchNfa (fromReg re) "cccccd" `shouldBe` False
