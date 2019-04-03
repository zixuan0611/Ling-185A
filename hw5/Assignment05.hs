{-# LANGUAGE FlexibleInstances #-}

module Assignment05 where

import Prelude hiding (init)

import SemiringFSA

data Numb = Z | S Numb deriving Show

distrib_lhs :: (Semiring a) => a -> a -> a -> a
distrib_lhs x y z = gconj x (gdisj y z)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

distrib_rhs :: (Semiring a) => a -> a -> a -> a
distrib_rhs x y z = gdisj (gconj x y) (gconj x z)

dotprod :: (Semiring v) => [v] -> [v] -> v
dotprod seq1 seq2 = case seq1 of 
                    [] -> gfalse
                    x:xrest -> case seq2 of
                               [] -> gfalse
                               y:yrest -> gdisj (gconj x y) (dotprod xrest yrest)

expn :: (Semiring a) => a -> Numb -> a
expn ele n = case n of
             Z -> gtrue
             S r -> gconj ele (expn ele r)

backward :: (Semiring a, Ord st, Ord sy) => GenericAutomaton st sy a -> [sy] -> st -> a
backward gfsa str q = 
    let (starts, ends, trs) = gfsa in
    case str of
    [] -> fin gfsa q
    x:xrest -> big_gdisj [gconj (tr gfsa q x q') (backward gfsa xrest q') | q' <- allStates gfsa]

val :: (Semiring a, Ord st, Ord sy) => GenericAutomaton st sy a -> [sy] -> a
val gfsa str = 
    let (starts, ends, trs) = gfsa in big_gdisj [gconj (init gfsa q) (backward gfsa str q) | q <- allStates gfsa]

addCost :: Cost -> Cost -> Cost
addCost c1 c2 = case c1 of
                Inf -> Inf
                TheInt x -> case c2 of
                            Inf -> Inf
                            TheInt y -> TheInt (x + y)

minCost :: Cost -> Cost -> Cost
minCost c1 c2 = case c1 of
                Inf -> c2
                TheInt x -> case c2 of
                            Inf -> c1
                            TheInt y -> case (x < y) of {True -> c1; False -> c2}

instance Semiring Cost where
    gconj x y = addCost x y
    gdisj x y = minCost x y
    gtrue = TheInt 0
    gfalse = Inf

instance Semiring [String] where
    gconj r1 r2 = [u ++ v | u <- r1, v <- r2]
    gdisj r1 r2 = concat [r1, r2]
    gtrue = [[]]
    gfalse = []

gfsa4 :: GenericAutomaton Int Char [String]
gfsa4 =
    ([(0, [[]])],
     [(0, [[]]),
      (1, [[]]),
      (2, ["t"])],
     [(0, 'n', ["n"], 0),
      (0, 't', ["t"], 0),
      (0, 'a', ["a"], 1),
      (1, 'n', ["n"], 0),
      (1, 'a', ["a"], 1),
      (1, 't', [[]], 2),
      (2, 'a', ["ta", "Ta"], 1),
      (2, 'n', ["tn"], 0),
      (2, 't', ["tt"], 0)]
    )

