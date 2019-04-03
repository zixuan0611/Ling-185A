module Assignment03 where

import FiniteState

data SegmentPKIU = P | K | I | U | WB deriving (Eq, Ord, Show)

------------------------------------------------------------------
------------------------------------------------------------------
-- IMPORTANT: Please do not change anything above here.
--            Write all your code below this line.

fsa_asst1 :: Automaton Int Bool
fsa_asst1 = (54, [38], [(54, False, 54),
                        (54, True, 73),
                        (73, False, 73),
                        (73, True, 21),
                        (21, False, 21),
                        (21, True, 54),
                        (21, True, 38),
                        (38, False, 38)])

fsa_asst2a :: Automaton Int SegmentCV
fsa_asst2a = (1, [3], [(1, V, 1),
                       (1, C, 2),
                       (2, V, 2),
                       (2, C, 3),
                       (3, V, 3),
                       (3, C, 3)])

fsa_asst2b :: Automaton (Int, Int) SegmentCV
fsa_asst2b = ((1, 3), [(2, 3)], [((1, 3), V, (1, 4)),
                                 ((1, 4), V, (1, 3)),
                                 ((1, 4), C, (2, 4)),
                                 ((2, 4), C, (1, 4)),
                                 ((2, 4), V, (2, 3)),
                                 ((2, 3), V, (2, 4)),
                                 ((2, 3), C, (1, 3)),
                                 ((1, 3), C, (2, 3))])

fsa_asst2c :: Automaton Int SegmentCV
fsa_asst2c = (1, [4], [(1, C, 2),
                       (1, V, 2),
                       (2, C, 3),
                       (2, V, 3),
                       (3, C, 4),
                       (4, C, 4),
                       (4, V, 4)])

fsa_asst2d :: Automaton Int SegmentCV
fsa_asst2d = (1, [4], [(1, C, 1),
                       (1, V, 1),
                       (1, C, 2),
                       (2, C, 3),
                       (2, V, 3),
                       (3, C, 4),
                       (3, V, 4)])

fsa_asst2e :: Automaton Int SegmentPKIU
fsa_asst2e = (1, [1, 2, 3], [(1, P, 2),
                             (1, K, 2),
                             (1, I, 2),
                             (1, WB, 2),
                             (2, P, 2),
                             (2, K, 2),
                             (2, I, 2),
                             (2, WB, 2),
                             (2, WB, 3),
                             (3, WB, 2),
                             (3, P, 3),
                             (3, K, 3),
                             (3, U, 3),
                             (3, WB, 3),
                             (1, P, 3),
                             (1, K, 3),
                             (1, U, 3),
                             (1, WB, 3)])

fsa_asst2f :: Automaton Int SegmentPKIU
fsa_asst2f = (1, [1, 2], [(1, P, 1),
                          (1, K, 1),
                          (1, I, 1),
                          (1, P, 2),
                          (2, P, 2),
                          (2, K, 2),
                          (2, I, 2),
                          (2, U, 2)])

fsa_asst2g :: Automaton Int SegmentPKIU
fsa_asst2g = (1, [1, 2], [(1, P, 1),
                          (1, K, 1),
                          (1, I, 1),
                          (1, P, 2),
                          (2, P, 1),
                          (2, K, 1),
                          (2, I, 1),
                          (2, U, 1)])

fsa_asst2h :: Automaton Int SegmentPKIU
fsa_asst2h = (1, [3, 5], [(1, P, 1),
                          (1, K, 1),
                          (1, I, 1),
                          (1, U, 1),
                          (1, P, 2),
                          (2, P, 2),
                          (2, K, 2),
                          (2, I, 2),
                          (2, U, 2),
                          (2, U, 3),
                          (3, P, 3),
                          (3, K, 3),
                          (3, I, 3),
                          (3, U, 3),
                          (1, U, 4),
                          (4, P, 4),
                          (4, K, 4),
                          (4, I, 4),
                          (4, U, 4),
                          (4, P, 5),
                          (5, P, 5),
                          (5, K, 5),
                          (5, I, 5),
                          (5, U, 5)])
