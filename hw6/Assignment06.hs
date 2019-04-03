module Assignment06 where

import CFG

lhs :: RewriteRule nt t -> nt
lhs r = case r of
        NontermRule x (y,z) -> x
        TermRule x y -> x

rhs :: RewriteRule nt t -> [Symbol nt t]
rhs r = case r of
        NontermRule x (y,z) -> [NT y, NT z]
        TermRule x y -> [T y]

--------------------------------
---- Natural numbers

data Numb = Z | S Numb deriving Show

(one, two, three, four, five, six, seven, eight, nine, ten)
    = (S Z, S one, S two, S three, S four, S five, S six, S seven, S eight, S nine)   -- a bit sneaky

add :: Numb -> Numb -> Numb
add n m = case n of {Z -> m; S n' -> add n' (S m)}

--------------------------------
---- Two more example grammars

-- Equivalent to (3) on the handout
cfg_anbn :: CFG Int Char
cfg_anbn = (0, 
            [NontermRule 0 (10, 1), 
             NontermRule 1 (0, 11), 
             NontermRule 0 (10,11), 
             TermRule 10 'a', 
             TermRule 11 'b'
            ]
           )

-- Equivalent to (11) on the handout
data Cat = NP | X | CNJ deriving (Show, Eq)
cfg_ambiguity :: CFG Cat String
cfg_ambiguity = (NP, 
                 [NontermRule NP (NP, X), 
                  NontermRule X (CNJ, NP), 
                  TermRule NP "apples", 
                  TermRule NP "bananas", 
                  TermRule NP "oranges", 
                  TermRule CNJ "and", 
                  TermRule CNJ "or"
                 ]
                )

--------------------------------
---- Trees to represent analyses

data Tree nt t = Leaf nt t | NonLeaf nt (Tree nt t) (Tree nt t) deriving Show

root :: Tree nt t -> nt
root tree =
    case tree of
    Leaf x y -> x
    NonLeaf x t1 t2 -> x

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

terminalsOnly :: [Symbol nt t] -> Maybe [t]
terminalsOnly l = case l of
                  [] -> Just []
                  x : rest -> case x of
                              NT nt -> Nothing
                              T t -> combine (Just [t]) (terminalsOnly rest)

combine :: Maybe [t] -> Maybe [t] -> Maybe [t]
combine j1 j2 = case j1 of
                Nothing -> Nothing
                Just xs -> case j2 of
                           Nothing -> Nothing
                           Just ys -> Just (xs ++ ys)

yield :: Tree nt t -> [t]
yield tr = case tr of
           Leaf x y -> [y]
           NonLeaf a b c -> (yield b) ++ (yield c)

treeToRuleList :: Tree nt t -> [RewriteRule nt t]
treeToRuleList tr = case tr of
                    Leaf x y -> [(TermRule x y)]
                    NonLeaf a b c -> [(NontermRule a ((daughter b),(daughter c)))] ++ (treeToRuleList b) ++ (treeToRuleList c)

daughter :: Tree nt t -> nt
daughter tr = case tr of
              Leaf x y -> x
              NonLeaf a b c -> a

ruleListToTree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Maybe (Tree nt t)
ruleListToTree rl = if (validrule rl) then
                       Just (generatetree rl)
                    else
                       Nothing

validrule :: (Eq nt, Eq t) => [RewriteRule nt t] -> Bool
validrule rl = case rl of
               [] -> False
               x : rest -> case x of 
                           TermRule m n -> validrule rest
                           NontermRule a (b,c) -> ((validhelp b rl) && (validhelp c rl))

validhelp :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> Bool
validhelp m n = case n of
                [] -> False
                s : rest -> case s of
                            TermRule x y -> if m == x then
                                               True
                                            else
                                               validhelp m rest
                            NontermRule a b -> if m == a then
                                                  True && (validrule n)
                                                else
                                                  validhelp m rest

generatetree :: (Eq nt, Eq t) => [RewriteRule nt t] -> Tree nt t
generatetree rl = case rl of 
                  s : rest -> case s of
                              TermRule x y -> Leaf x y
                              NontermRule a (b,c) -> NonLeaf a (generatehelp b rest) (generatehelp c rest)

generatehelp :: (Eq nt, Eq t) => nt -> [RewriteRule nt t] -> Tree nt t
generatehelp m n = case n of
                   s : rest -> case s of
                               TermRule x y -> if m == x then
                                                  (Leaf x y)
                                                else
                                                  generatehelp m rest
                               NontermRule a b -> if m == a then
                                                     generatetree ([(NontermRule a b)] ++ rest)
                                                  else
                                                     generatehelp m rest

splitAtLeftmost :: (Eq nt, Eq t) => [Symbol nt t] -> Maybe ([Symbol nt t], nt, [Symbol nt t])
splitAtLeftmost l = case l of
                    [] -> Nothing
                    (x:xs) -> case x of
                              NT nt -> Just ([], nt, xs)
                              T t -> case (splitAtLeftmost xs) of
                                     Nothing -> Nothing
                                     Just (ys, ns, zs) -> Just (x:ys, ns, zs)


                                 
