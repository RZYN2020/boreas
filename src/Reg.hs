module Reg( Reg(..)) where

data Reg c
  =
    Empty 
  | Epsilon
  | Literal c
  | Con (Reg c) (Reg c)
  | Alt (Reg c) (Reg c)
  | Star (Reg c)
  deriving Eq

showReg :: Show a => Reg a -> String
showReg Empty = "empty"
showReg Epsilon = "epsilon"
showReg (Literal c) = show c
showReg (Con r r') = "(" ++ showReg r ++ showReg r' ++ ")"
showReg (Alt r r') = "(" ++ showReg r ++ "|" ++ showReg r' ++ ")"
showReg (Star r) = showReg r ++ "*"


instance Show c => Show (Reg c) where
    show = showReg

