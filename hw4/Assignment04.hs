module Assignment04 where

import FiniteState
import FiniteStatePart2

---------------------------------------
-- Setup for section 2

type SLG sy = ([sy], [sy], [(sy,sy)])
data ConstructedState sy = ExtraState | StateForSymbol sy deriving (Eq, Ord, Show)

slg1 :: SLG SegmentCV
slg1 = ([C], [V], [(C,C),(C,V),(V,V)])

slg2 :: SLG Int
slg2 = ([1,2,3], [1,2,3], [(1,1),(2,2),(3,3),(1,2),(2,1),(1,3),(3,1)])

---------------------------------------
-- Setup for section 3

data RegExp sy = Lit sy
               | Alt (RegExp sy) (RegExp sy)
               | Concat (RegExp sy) (RegExp sy)
               | Star (RegExp sy)
               | ZeroRE
               | OneRE
               deriving Show

re1 :: RegExp Char
re1 = Concat (Alt (Lit 'a') (Lit 'b')) (Lit 'c')

re2 :: RegExp Char
re2 = Star re1

re3 :: RegExp Int
re3 = Star (Concat ZeroRE (Lit 3))

re4 :: RegExp Int
re4 = Concat (Alt (Lit 0) (Lit 1)) (Star (Lit 2))

-- Produces a new version of an FSA with the guarantee that certain 
-- integers are not used as state labels.
ensureUnused :: [Int] -> EpsAutomaton Int sy -> EpsAutomaton Int sy
ensureUnused reserved fsa =
    if reserved == [] then
        fsa
    else
        -- nonneg maps integers to non-negative integers, preserving all distinctions
        let nonneg x = if x < 0 then 2*(-x)-1 else 2*x in
        -- create a version of fsa where all state numbers are non-negative
        let fsanonneg = mapStates nonneg fsa in
        -- now add enough to all state numbers to make sure they don't clash with the reserved list
        mapStates (\x -> x + 1 + maximum reserved) fsanonneg

-- Adjusts the state labels throughout an FSA
mapStates :: (a -> b) -> EpsAutomaton a sy -> EpsAutomaton b sy
mapStates f (start, ends, delta) =
    let newDelta = map (\(q1,x,q2) -> (f q1, x, f q2)) delta in
    (f start, map f ends, newDelta)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

backwardCheck :: (Ord st, Ord sy) => Automaton st sy -> [sy] -> st -> Bool
backwardCheck fsa str q = 
    let (start, ends, delta) = fsa in
    let states = allStates fsa in
    case str of
    [] -> elem q ends
    x : r -> or (map (\q' -> backwardCheck fsa r q' && elem (q, x, q') delta) states)

recognizeSLG :: (Ord sy) => SLG sy -> [sy] -> Bool
recognizeSLG slg str =
    let (start, ends, delta) = slg in
    case str of
    [] -> False -- note that slg does not allow empty strings
    x : r -> recognizeSLGhelper slg str && elem x start

recognizeSLGhelper :: (Ord sy) => SLG sy -> [sy] -> Bool
recognizeSLGhelper slg str =
    let (start, ends, delta) = slg in
    case str of
    [] -> True
    x1 : r1 -> case r1 of
              [] -> elem x1 ends
              x2 : r2 -> recognizeSLGhelper slg r1 && elem (x1, x2) delta

slgToFSA :: SLG sy -> Automaton (ConstructedState sy) sy
slgToFSA slg = 
    let (start, ends, delta) = slg in
    let sstate = ExtraState in
    let estates = map (\p -> StateForSymbol p) ends in
    let sdelta = map (\q -> (ExtraState, q, StateForSymbol q)) start in
    let rdelta = map (\(s1, s2) -> (StateForSymbol s1, s2, StateForSymbol s2)) delta in (sstate, estates, sdelta ++ rdelta)

unionFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy -> EpsAutomaton Int sy
unionFSAs fsa1 fsa2 = 
    let sstate = 66 in
    let (start1, ends1, delta1) = ensureUnused [sstate] fsa1 in
    let (start2, ends2, delta2) = ensureUnused ([sstate] ++ (allStates fsa1)) fsa2 in
    let start = sstate in
    let ends = ends1 ++ ends2 in
    let sdelta = [(start, Nothing, start1), (start, Nothing, start2)] in
    let delta = delta1 ++ delta2 ++ sdelta in (start, ends, delta)

concatFSAs :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy -> EpsAutomaton Int sy
concatFSAs fsa1 fsa2 =
    let (start1, ends1, delta1) = fsa1 in
    let (start2, ends2, delta2) = ensureUnused (allStates fsa1) fsa2 in
    let start = start1 in
    let ends = ends2 in
    let edelta = map (\es -> (es, Nothing, start2)) ends1 in
    let delta = delta1 ++ delta2 ++ edelta in (start, ends, delta)

starFSA :: (Ord sy) => EpsAutomaton Int sy -> EpsAutomaton Int sy
starFSA fsa =
    let sstate = 88 in
    let (s, e, d) = ensureUnused [sstate] fsa in
    let start = sstate in
    let ends = e ++ [start] in
    let sdelta = [(start, Nothing, s)] in
    let rdelta = map (\es -> (es, Nothing, s)) e in
    let delta = d ++ sdelta ++ rdelta in (start, ends, delta)

reToFSA :: (Ord sy) => RegExp sy -> EpsAutomaton Int sy
reToFSA fsa =
    let reiFuruya = 100 in
    let ichiKudo = 101 in
    let startlit = 102 in
    let endslit = 103 in
    let m = [startlit, endslit, reiFuruya, ichiKudo] in
            case fsa of
            Lit x -> (startlit, [endslit], [(startlit, Just x, endslit)])
            Alt r1 r2 -> unionFSAs (ensureUnused m (reToFSA r1)) (ensureUnused m (reToFSA r2))
            Concat r1 r2 -> concatFSAs (ensureUnused m (reToFSA r1)) (ensureUnused m (reToFSA r2))
            Star r -> starFSA (ensureUnused m (reToFSA r))
            ZeroRE -> (reiFuruya, [], [])
            OneRE -> (ichiKudo, [ichiKudo], [])
