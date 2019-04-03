{-# LANGUAGE FlexibleInstances #-}

module Assignment07 where

import Prelude hiding (init)

import SemiringCFG

-------------------------------------
-- probToBool is described at the 
-- beginning of section 2

probToBool :: GenericCFG nt t Double -> GenericCFG nt t Bool
probToBool (starts, rules) =
    ([(c, p > 0) | (c,p) <- starts], [(r, p > 0) | (r,p) <- rules])

--------------------------------
-- setup for questions F and G

data Cost = TheInt Int | Inf deriving (Show, Eq)

isCompoundRule :: (Eq nt) => RewriteRule nt t -> Bool
isCompoundRule (NontermRule c (c1,c2)) = c == c1 && c == c2
isCompoundRule (TermRule c x) = False

isAdjunctRule :: (Eq nt) => RewriteRule nt t -> Bool
isAdjunctRule (NontermRule c (c1,c2)) = (c == c1 || c == c2) && (c1 /= c2)
isAdjunctRule (TermRule c x) = False

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

val :: (Ord nt, Ord t, Semiring a) => GenericCFG nt t a -> [t] -> a
val cfg s = big_gdisj [gconj (init cfg x) (fastInside cfg s x) | x <- (allNTs cfg)]

instance Semiring Int where
    gconj x y = x * y
    gdisj x y = x + y
    gtrue = 1
    gfalse = 0

boolToCount :: GenericCFG nt t Bool -> GenericCFG nt t Int
boolToCount (starts, rules) = 
    ([case (c, p) of {(c, True) -> (c, gtrue); (c, False) -> (c, gfalse)} | (c,p) <- starts], [case (r, p) of {(r, True) -> (r, 1); (r, False) -> (r, 0)}  | (r,p) <- rules])

instance Semiring [Double] where
    gconj x y = [u * v | u <- x, v <- y]
    gdisj x y = x ++ y
    gtrue = [1.0]
    gfalse = []

probToProbList :: GenericCFG nt t Double -> GenericCFG nt t [Double]
probToProbList (starts, rules) = 
    ([(c, [p]) | (c,p) <- starts], [(r, [p]) | (r,p) <- rules])

instance Semiring [[RewriteRule nt t]] where
    gconj x y = [u ++ v | u <- x, v <- y]
    gdisj x y = concat [x, y]
    gtrue = [[]]
    gfalse = []

boolToDerivList :: GenericCFG nt t Bool -> GenericCFG nt t [[RewriteRule nt t]]
boolToDerivList (starts, rules) =
    ([case (c, p) of {(c, True) -> (c, gtrue); (c, False) -> (c, gfalse)} | (c,p) <- starts], [case (r, p) of {(r, True) -> (r, [[r]]); (r, False) -> (r, [])} | (r,p) <- rules])

--instance Semiring [(double, [RewriteRule nt t])] where
--   gconj x y = case x of
--               [] -> []
--               m : mrest -> case y of
--                            [] -> [x]
--                            n:nrest -> (gconj (fst m) (fst n), (snd m) ++ (snd n)) ++ (gconj mrest nrest)


--probToProbDerivList :: GenericCFG nt t Double -> GenericCFG nt t [(Double, [RewriteRule nt t])]
--probToProbDerivList

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

boolToLowestCost :: (Eq nt) => GenericCFG nt t Bool -> GenericCFG nt t Cost
boolToLowestCost (starts, rules) =
    ([case (c, p) of {(c, True) -> (c, gtrue); (c, False) -> (c, gfalse)} | (c,p) <- starts], [case (r, p) of {(r, True) -> if (isCompoundRule r) then (r, TheInt 0)
                                                                                                                            else if (isAdjunctRule r) then (r, TheInt 1)
                                                                                                                            else (r, TheInt 3);
                                                                                                               (r, False) -> if (isCompoundRule r) then (r, TheInt 0)
                                                                                                                             else if (isAdjunctRule r) then (r, TheInt 1)
                                                                                                                             else (r, TheInt 3)}  | (r,p) <- rules])

instance Semiring (Cost, [[RewriteRule nt t]]) where
    gconj x y = (addCost (fst x) (fst y), [u ++ v | u <- snd x, v <- snd y])
    gdisj x y = (minCost (fst x) (fst y), concat [snd x, snd y])
    gtrue = (TheInt 0, [[]])
    gfalse = (Inf, [])

boolToLowestCostDerivs :: (Eq nt) => GenericCFG nt t Bool -> GenericCFG nt t (Cost, [[RewriteRule nt t]])
boolToLowestCostDerivs (starts, rules) =
    ([case (c, p) of {(c, True) -> (c, gtrue); (c, False) -> (c, gfalse)} | (c,p) <- starts], [case (r, p) of {(r, True) -> if (isCompoundRule r) then (r, (TheInt 0, [[r]]))
                                                                                                                            else if (isAdjunctRule r) then (r, (TheInt 1, [[r]]))
                                                                                                                            else (r, (TheInt 3, [[r]]));
                                                                                                               (r, False) -> if (isCompoundRule r) then (r, (TheInt 0, []))
                                                                                                                             else if (isAdjunctRule r) then (r, (TheInt 1, []))
                                                                                                                             else (r, (TheInt 3,[]))}  | (r,p) <- rules])

