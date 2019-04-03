{-# LANGUAGE FlexibleInstances #-}

module FinalProject where

-- the final project is inspired by Assignment05 and we need some functions from it
import Assignment05

-- we are applying the following type for phonological rules in this project
type PhonologicalRule = (Char, Char, Maybe Char, Maybe Char)
--the first argument denotes the character to be changed
--the second argument denotes the character after the change
--the third and fourth compenents decribe the context requrements:
--the third argument denotes the character before the character to be replaced
--the forth argument denotes the character after the character to be replaced
--Note that the third and fourth argument are of type Maybe Char since some phonological rules only care about one side

-- here is a list of symbols we may be used for our project
allList = ['?','#','a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z']

-- here is a list of assumed phonological rules we are going to explore:
--1. Assimilation:
assimilation = ('n', 'm', Nothing, Just 'b')
--2. Glottalization:
glottalization = ('t', '?', Nothing, Just 'n')
--3. Devoicing:
devoicing = ('b', 'p', Nothing, Just '#')
--4. Voicing:
voicing = ('f', 'v', Just 'i', Just 'e')
--5. Vowel Lowering
vowelLowering = ('i', 'e', Just 'l', Nothing)
--6. initial voicing
initialVoicing = ('s', 'z', Just '#', Nothing)

-- this is our type for automata with the possibility of computing
-- we will use this type for our transducers of phonological rules
type GenericAutomaton st sy v = ([(st,v)], [(st,v)], [(st,sy,v,st)])

-- this is what I write for the last question of Assignment05
-- this is also an important illustration of what our transducers look like
-- the finite state transducers for the rule when "t" optinally becomes a flap
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

-- this is our function to produce the finite state transducers
phonologicalDerivation :: PhonologicalRule -> GenericAutomaton Int Char [String]
phonologicalDerivation r = 
  let (f, s, before, after) = r in
  let bef = case before of {Just b -> [b]; Nothing -> [e | e <- allList, e/=f]} in
  let aft = case after of {Just a -> [a]; Nothing -> [t | t <- allList, t/=f]} in
      ([(0, [[]])],
       [(0, [[]]),
        (1, [[]]),
        (2, [[f]])],
       [(0, p, [[p]], 0) | p <- allList, not(elem p bef)]
       ++[(0, o, [[o]], 1) | o <- bef]
       ++[(1, q, [[q]], 0) | q <- allList, q /= f && not (elem q bef)]
       ++[(1, m, [[m]], 1) | m <- bef]
       ++[(1, f, [[]], 2)]
       ++[(2, n, [[f, n]], 0) | n <- allList, not (elem n aft)]
       ++[(2, c, [[s, c]], case (elem c bef) of {False -> 0; True -> 1}) | c <- aft])

--Note: in the two derivation functions below, we will apply the val function written in Assignment05
--Note: to keep consistence with the form of possible results produced by gfsa4, we are using [String] instead of String for the input and output
--(since val gfsa4 "String" produces [String])

-- this function takes a single phonological rule and an underlying form, then outputs possible surface forms
singleRuleDerivation :: PhonologicalRule -> [String] -> [String]
singleRuleDerivation r str = concat[val (phonologicalDerivation r) s | s <- str]

-- we will use a list of phonological rules in order to evaluate possible surface forms
-- note that the list of phonological rules must be in order
ruleOrderDerivation :: [PhonologicalRule] -> [String] -> [String]
ruleOrderDerivation r str =
  case r of
    [] -> str
    (pr:rest) -> ruleOrderDerivation rest (concat[val (phonologicalDerivation pr) s | s <- str])

-- this is a helpler function used to check if the evaluated surface form match the predicted
checkHelp :: [String] -> String -> Bool
checkHelp l s = case l of
                [] -> False
                (r:rest) -> if r == s then
                               True
                            else
                              checkHelp rest s

-- check if the produced string is equal to the predicted string
-- this function takes a single Phonological rule and an underlying form
-- then checks if the prediceted string match any of the possible surface form
oneRuleCheck :: PhonologicalRule -> [String] -> String -> Bool
oneRuleCheck r str1 str2 = case singleRuleDerivation r str1 of
                           [] -> False
                           (s:rest) -> if s == str2 then
                                          True
                                        else
                                          checkHelp rest str2

-- check if the produced string is equal to the predicted string
-- this function takes a list of Phonological rules and an underlying form
-- then checks if the prediceted string match any of the possible surface form
checkResults :: [PhonologicalRule] -> [String] -> String -> Bool
checkResults r str1 str2 = case ruleOrderDerivation r str1 of
                           [] -> False
                           (s:rest) -> if s == str2 then
                                          True
                                        else
                                          checkHelp rest str2
