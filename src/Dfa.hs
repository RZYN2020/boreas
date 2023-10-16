{-# LANGUAGE LambdaCase #-}

module Dfa(Dfa(..), minimize, fromNfa, matchDfa) where

import Data.Set
import Nfa (Nfa(..), closure, step, fixPoint)


-- a for state; b for charactor
type Trans a b = (a, b, a)

data Dfa a b = DFA
  { alphabet :: Set b,
    states :: Set a,
    initial :: a,
    final :: Set a,
    transitions :: Set (Trans a b)
  }
  deriving (Eq, Show)

matchDfa :: (Ord a, Eq b) => Dfa a b -> [b] -> Bool
matchDfa dfa cs =
  case r of 
    Nothing -> False
    Just st -> member st fi
  where
    r = runDfa dfa cs
    fi = Dfa.final dfa

runDfa :: (Ord a, Eq b) => Dfa a b -> [b] -> Maybe a
runDfa dfa = Prelude.foldl (Dfa.step dfa) (Just $ Dfa.initial dfa)

step :: (Eq a, Eq b) => Dfa a b -> Maybe a -> b -> Maybe a
step dfa st c = 
  case st of
    Nothing -> Nothing
    Just st' ->
      case nextof st' c of
        Nothing -> Nothing
        Just (_, _, stt') -> Just stt'
  where
    nextof st' c' =
      safeHead
        $ toList
        $ Data.Set.filter
          ( \case
              (stt, ct, _) | stt == st', ct == c' -> True
              _ -> False
          )
        $ Dfa.transitions dfa


-- Rabin–Scott powerset construction
-- https://en.wikipedia.org/wiki/Powerset_construction
fromNfa :: (Ord b, Ord a) => Nfa a b -> Dfa Int b
fromNfa  = transDfaState . nfaToDfa


nfaToDfa :: (Ord b, Ord a) => Nfa a b -> Dfa (Set a) b
nfaToDfa nfa = fixPoint go initialDfa
  where 
    initStates = Nfa.closure nfa $ singleton $ Nfa.initial nfa
    initialDfa = DFA {
      Dfa.alphabet = Nfa.alphabet nfa,
      Dfa.states = singleton initStates,
      Dfa.initial = initStates,
      Dfa.final = empty,
      Dfa.transitions = empty
    }
    -- every iter
    -- -- for every char in alphabet
    go dfa = slipr Data.Set.foldl dfa (Dfa.alphabet dfa) $ \acc c ->
    -- -- -- for every state in cur states
      slipr Data.Set.foldl acc (Dfa.states dfa) $ \acc' s ->
    -- -- -- -- DFA update
        let next = Nfa.step nfa s c
        in 
          DFA {
            Dfa.alphabet = Dfa.alphabet dfa,
            Dfa.states = Dfa.states acc' `union` singleton next,
            Dfa.initial = Dfa.initial dfa,
            Dfa.final = if not $ Data.Set.null $ intersection next (Nfa.final nfa) then Dfa.final acc' `union` singleton next else Dfa.final acc',
            Dfa.transitions = Dfa.transitions acc' `union` singleton (s, c, next)
        }


transDfaState :: (Ord b, Ord a) => Dfa (Set a) b -> Dfa Int b
transDfaState dfa
  = let 
      alphabet' = Dfa.alphabet dfa
      states' = Dfa.states dfa
      initial' = Dfa.initial dfa
      final' = Dfa.final dfa
      transitions' = Dfa.transitions dfa
      getIdx  = (`Data.Set.findIndex` states')
    in
    DFA {
      Dfa.alphabet = alphabet',
      Dfa.states = fromList [0..Data.Set.size states'],
      Dfa.initial = getIdx initial',
      Dfa.final = Data.Set.map getIdx final',
      Dfa.transitions = Data.Set.map (\(s, c, s') -> (getIdx s, c, getIdx s')) transitions'
    }


-- DFA minimization
-- https://en.wikipedia.org/wiki/DFA_minimization
minimize :: Dfa a b -> Dfa a b
minimize = undefined


slipr :: (a -> b -> c -> d) -> b -> c -> a -> d
slipr f b c a = f a b c

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x