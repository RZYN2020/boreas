module Reg (Reg (..), parseReg) where

import Control.Applicative ((<|>))
import Text.Parsec (char, letter, many1, parse, string)
import Text.Parsec.Expr
  ( Assoc (AssocLeft),
    Operator (Infix, Postfix),
    buildExpressionParser,
  )
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

parens :: Parser (Reg Char) -> Parser (Reg Char)
parens p = do
  _ <- char '('
  r <- p
  _ <- char ')'
  return r

epsilon :: Parser (Reg Char)
epsilon = do
  return Epsilon

reservedOp :: String -> Parser ()
reservedOp s = do
  _ <- string s
  return ()

manyLiteral :: Parser (Reg Char)
manyLiteral = do
  cs <- many1 letter
  return $ foldl1 Con $ map Literal cs

reg :: Parser (Reg Char)
reg = buildExpressionParser table term
  where
    term = parens reg <|> manyLiteral <|> epsilon
    table =
      [ [postfix "*" Star],
        [binary "" Con AssocLeft],
        [binary "|" Alt AssocLeft]
      ]
    binary name fun = Infix (do reservedOp name; return fun)
    postfix name fun = Postfix (do reservedOp name; return fun)

parseReg :: String -> Reg Char
parseReg s = case parse reg "" s of
  Left err -> error $ show err
  Right r -> r

instance (Show c) => Show (Reg c) where
  show = showReg
