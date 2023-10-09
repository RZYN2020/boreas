{-# LANGUAGE LambdaCase #-}

module Nfa where
import Data.Set
import Reg

data Trans st c
  = EpiTrans st st
  | OneTrans st c st
  deriving (Ord, Eq, Show)

data Nfa st c = NFA
  { alphabet :: Set c,
    states :: Set st,
    initial :: st,
    final :: Set st,
    transitions :: Set (Trans st c)
  }
  deriving (Eq, Show)

matchNfa :: (Ord st, Eq c) => Nfa st c -> [c] -> Bool
matchNfa nfa cs =
  not (Data.Set.null (intersection rs fi))
  where
    rs = runNfa nfa cs
    fi = final nfa

runNfa :: (Ord st, Eq c) => Nfa st c -> [c] -> Set st
runNfa nfa = Prelude.foldl (step nfa) (closure nfa $ singleton $ initial nfa)

step :: (Ord st, Eq c) => Nfa st c -> Set st -> c -> Set st
step nfa s ch = closure nfa $ oneMove nfa s ch

oneMove :: (Ord st, Eq c) => Nfa st c -> Set st -> c -> Set st
oneMove nfa sts ch =
  Data.Set.foldl' (\acc s -> acc `union` nextof s ch) empty sts
  where
    transof state ch' =
      Data.Set.filter
        ( \case
            OneTrans s c _ | s == state, c == ch' -> True
            _ -> False
        )
        $ transitions nfa
    nextof state ch' =
      Data.Set.map
        ( \case
            OneTrans _ _ s' -> s'
            _ -> undefined
        )
        $ transof state ch'

fixPoint :: Eq t => (t -> t) -> t -> t
fixPoint f x
  | x == r = r
  | otherwise = fixPoint f r
  where
    r = f x

-- closure to to a limit
closure :: (Ord st) => Nfa st c -> Set st -> Set st
closure nfa =
  fixPoint iter
  where
    eptransof state =
      Data.Set.filter
        ( \case
            EpiTrans s _ | s == state -> True
            _ -> False
        )
        $ transitions nfa
    epnextof state =
      Data.Set.map
        ( \case
            EpiTrans _ s' -> s'
            _ -> undefined
        )
        $ eptransof state
    iter set =
      Data.Set.foldl' (\acc s' -> acc `union` epnextof s') empty set
        `union` set

-- Thompson’s Algorithm
fromReg :: (Ord c) => Reg c -> Nfa Int c
fromReg Empty = NFA empty (fromList [0, 1]) 0 (singleton 1) empty
fromReg Epsilon = NFA empty (fromList [0, 1]) 0 (singleton 1) (singleton $ EpiTrans 0 1)
fromReg (Literal ch) =
  NFA
    { alphabet = singleton ch,
      states = fromList [0, 1],
      initial = 0,
      final = singleton 1,
      transitions = singleton $ OneTrans 0 ch 1
    }
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