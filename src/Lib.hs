module Lib (Reg (..), Nfa(..)) where

import Control.Applicative
  ( Alternative (empty, (<|>)),
    Applicative (liftA2),
  )

import Reg
import Nfa

newtype RegEng tok res = RegEng { match :: [tok] -> Maybe (res, [tok])}

-- instance Functor (RegEng t) where
--   fmap f r = RegEng $
--     \s -> case match r s of
--       Nothing -> Nothing
--       Just (x, xs) -> Just (f x, xs)

-- instance Applicative (RegEng t) where
--   pure x = RegEng $ \s -> Just (x, s)
--   rf <*> r = RegEng $
--     \s -> case match rf s of
--       Nothing -> Nothing
--       Just (f, fs) -> case match r fs of
--         Nothing -> Nothing
--         Just (x, xs) -> Just (f x, xs)

-- instance Monad (RegEng t) where
--   return = pure
--   r >>= f = RegEng $
--     \s -> case match r s of
--       Nothing -> Nothing
--       Just (x, xs) -> match (f x) xs

-- instance Alternative (RegEng t) where
  -- empty = RegEng (const Nothing)
  -- r <|> r' = RegEng $
  --   \s -> case match r s of
  --     Nothing -> match r' s
  --     res -> res