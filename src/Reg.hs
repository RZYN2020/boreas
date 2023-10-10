module Reg (Reg (..), parseReg) where

import Control.Applicative
import Text.Parsec (anyChar, between, char, optionMaybe, parse, try)
import Text.Parsec.String (Parser)

data Reg c
  = Empty
  | Epsilon
  | Literal c
  | Con (Reg c) (Reg c)
  | Alt (Reg c) (Reg c)
  | Star (Reg c)
  deriving (Eq)

showReg :: (Show a) => Reg a -> String
showReg Empty = "empty"
showReg Epsilon = "epsilon"
showReg (Literal c) = show c
showReg (Con r r') = "(" ++ showReg r ++ showReg r' ++ ")"
showReg (Alt r r') = "(" ++ showReg r ++ "|" ++ showReg r' ++ ")"
showReg (Star r) = showReg r ++ "*"

-- Parser for a regular expression
reg :: Parser (Reg Char)
reg = undefined


parseReg :: String -> Reg Char
parseReg s = case parse reg "" s of
  Left err -> error $ show err
  Right r -> r

instance (Show c) => Show (Reg c) where
  show = showReg
