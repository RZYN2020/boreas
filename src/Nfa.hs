{-# LANGUAGE LambdaCase #-}

module Nfa where

import Data.Set
  ( Set,
    empty,
    foldl,
    foldl',
    fromList,
    insert,
    intersection,
    map,
    singleton,
    union,
  )
import Reg (Reg (..))

-- a for state; b for charactor
data Trans a b
  = EpiTrans a a
  | OneTrans a b a
  deriving (Ord, Eq, Show)

data Nfa a b = NFA
  { alphabet :: Set b,
    states :: Set a,
    initial :: a,
    final :: Set a,
    transitions :: Set (Trans a b)
  }
  deriving (Eq, Show)

matchNfa :: (Ord a, Eq b) => Nfa a b -> [b] -> Bool
matchNfa nfa cs =
  not $ null $ intersection rs fi
  where
    rs = runNfa nfa cs
    fi = final nfa

runNfa :: (Ord a, Eq b) => Nfa a b -> [b] -> Set a
runNfa nfa = Prelude.foldl (step nfa) (closure nfa $ singleton $ initial nfa)

step :: (Ord a, Eq b) => Nfa a b -> Set a -> b -> Set a
step nfa sts c = closure nfa $ oneMove nfa sts c

oneMove :: (Ord a, Eq b) => Nfa a b -> Set a -> b -> Set a
oneMove nfa sts c =
  Data.Set.foldl' (\acc s -> acc `union` nextof s c) empty sts
  where
    nextof st c' =
      mapMaybe
        ( \case
            OneTrans stt ct stt' | stt == st, ct == c' -> Just stt'
            _ -> Nothing
        )
        $ transitions nfa

-- closure to to a limit
closure :: (Ord a) => Nfa a b -> Set a -> Set a
closure nfa =
  fixPoint go
  where
    epnextof st =
      mapMaybe
        ( \case
            EpiTrans stt stt' | stt == st -> Just stt'
            _ -> Nothing
        )
        $ transitions nfa
    go set =
      Data.Set.foldl' (\acc s' -> acc `union` epnextof s') empty set
        `union` set

-- Thompson’s Algorithm
fromReg :: (Ord b) => Reg b -> Nfa Int b
fromReg Empty = NFA empty (fromList [0, 1]) 0 (singleton 1) empty
fromReg Epsilon = NFA empty (fromList [0, 1]) 0 (singleton 1) (singleton $ EpiTrans 0 1)
fromReg (Literal ch) = NFA (singleton ch) (fromList [0, 1]) 0 (singleton 1) (singleton $ OneTrans 0 ch 1)
fromReg (Con r r') =
  NFA
    { alphabet = a `union` a',
      states = s `union` renum s' l,
      initial = 0,
      final = singleton $ l + l' - 1,
      transitions =
        t
          `union` renumTrans t' l
          `union` singleton (EpiTrans (l - 1) l)
    }
  where
    (NFA a s _ _ t) = fromReg r
    (NFA a' s' _ _ t') = fromReg r'
    l = length s
    l' = length s'
fromReg (Alt r r') =
  NFA
    { alphabet = a `union` a',
      states = renum s 1 `union` renum s' (l + 1) `union` fromList [0, l + l'],
      initial = 0,
      final = singleton $ l + l',
      transitions =
        renumTrans t 1
          `union` renumTrans t' (l + 1)
          `union` fromList
            [ EpiTrans 0 1,
              EpiTrans 0 (l + 1),
              EpiTrans l (l + l'),
              EpiTrans (l + l' - 1) (l + l')
            ]
    }
  where
    (NFA a s _ _ t) = fromReg r
    (NFA a' s' _ _ t') = fromReg r'
    l = length s
    l' = length s'
fromReg (Star r) =
  NFA
    { alphabet = a,
      states = renum s 1 `union` fromList [0, l + 1],
      initial = 0,
      final = singleton $ l + 1,
      transitions =
        renumTrans t 1
          `union` fromList
            [ EpiTrans 0 1,
              EpiTrans 0 (l + 1),
              EpiTrans (l + 1) 0,
              EpiTrans l (l + 1)
            ]
    }
  where
    (NFA a s _ _ t) = fromReg r
    l = length s

-- Just two instances ... no need to use typeclass to represent them in the same renum
renum :: Set Int -> Int -> Set Int
renum s i = Data.Set.map (+ i) s

renumTrans :: (Ord c) => Set (Trans Int c) -> Int -> Set (Trans Int c)
renumTrans s i =
  Data.Set.map
    ( \case
        EpiTrans st st' -> EpiTrans (st + i) (st' + i)
        OneTrans st ch st' -> OneTrans (st + i) ch (st' + i)
    )
    s

--- Helper Functions
-- https://www.reddit.com/r/haskell/comments/2090x3/ask_rhaskell_why_is_there_no_functor_instance_for/
mapMaybe :: (Ord b) => (a -> Maybe b) -> Set a -> Set b
mapMaybe f =
  Data.Set.foldl
    ( \mp x -> case f x of
        Just x' -> insert x' mp
        Nothing -> mp
    )
    empty

fixPoint :: (Eq a) => (a -> a) -> a -> a
fixPoint f x
  | x == r = r
  | otherwise = fixPoint f r
  where
    r = f x