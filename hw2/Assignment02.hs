module Assignment02 where

-- Make some standard list functions unavailable. :-)
import Prelude hiding (length, map, filter, take, drop, head, tail, last, init, reverse, (!!))

import Recursion

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

mult :: Numb -> Numb -> Numb
mult = \m -> case m of
             Z -> \n -> Z
             S m' -> \n -> case n of
                           Z -> Z
                           S n' -> add m (mult n' m)

sumUpTo :: Numb -> Numb
sumUpTo = \n -> case n of
                Z -> Z
                S n' -> add n (sumUpTo n')

equal :: Numb -> Numb -> Bool
equal = \m -> case m of
              Z -> \n -> case n of
                         Z -> True
                         S n' -> False
              S m' -> \n -> case n of
                            Z -> False
                            S n' -> equal m' n'

count :: (a -> Bool) -> [a] -> Numb
count f l = case l of
            [] -> Z
            x : rest -> case (f x) of 
                       True -> (add one (count f rest))
                       False -> (count f rest)

listOf :: Numb -> a -> [a]
listOf = \n -> \l -> case n of 
                     Z -> []
                     S n' -> l : (listOf n' l)

addToEnd :: a -> [a] ->[a]
addToEnd = \n -> \l -> case l of
                       [] -> [n]
                       x : rest -> x : (addToEnd n rest)

remove :: (a -> Bool) -> [a] -> [a]
remove f l = case l of
             [] -> []
             x : rest -> case (f x) of
                         True -> (remove f rest)
                         False -> x : (remove f rest)

prefix :: Numb -> [a] -> [a]
prefix = \n -> \l -> case n of 
                     Z -> []
                     S n' -> case l of
                             [] -> []
                             x : rest -> x : (prefix n' rest)

countStars :: RegExp -> Numb
countStars r = case r of
               Lit x -> Z
               Alt r1 r2 -> (add (countStars r1) (countStars r2))
               Concat r1 r2 -> (add (countStars r1) (countStars r2))
               Star r -> (add one (countStars r))
               ZeroRE -> Z
               OneRE -> Z

depth :: RegExp -> Numb
depth r = case r of
          Lit x -> one
          Alt r1 r2 -> (add one (bigger (depth r1) (depth r2)))
          Concat r1 r2 -> (add one (bigger (depth r1) (depth r2)))
          Star r -> (add one (depth r))
          ZeroRE -> one
          OneRE -> one

reToString :: RegExp -> [Char]
reToString r = case r of
               Lit x -> [x]
               Alt r1 r2 -> "(" ++ (reToString r1) ++ "|" ++ (reToString r2) ++ ")"
               Concat r1 r2 -> "(" ++ (reToString r1) ++ "." ++ (reToString r2) ++ ")"
               Star r -> (reToString r) ++ "*"
               ZeroRE -> "0"
               OneRE -> "1"
