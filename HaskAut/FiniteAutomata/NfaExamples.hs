module HaskAut.FiniteAutomata.NfaExamples
  (module HaskAut.FiniteAutomata.NfaExamples, module Nfa,
   module ExampleBasics, module SubsetConstruction)
where

import HaskAut.Common.GvShow
import qualified HaskAut.FiniteAutomata.Dfa as Dfa
import HaskAut.FiniteAutomata.Dfa(Dfa(Dfa)) 
import HaskAut.FiniteAutomata.Nfa as Nfa
import HaskAut.FiniteAutomata.SubsetConstruction as SubsetConstruction
import HaskAut.FiniteAutomata.ExampleBasics as ExampleBasics

{- automaton from exercise 5.1 of Kozen -}
trans5'1 :: Trans Char Int
trans5'1 'a' = [(1,1),(2,3)]
trans5'1 'b' = [(1,1),(1,2),(2,3)]

ex5'1 :: Nfa Char Int
ex5'1 = Nfa [1, 2, 3] "ab" trans5'1 [1] [3]

run5'1 = multistep ex5'1 "aba"

accepting5'1 = accepts ex5'1 "baabb"
rejecting5'1 = accepts ex5'1 "baabab"

det5'1 = determinize ex5'1

daccepting5'1 = Dfa.accepts det5'1 "baabb"

----------------------------------------

data Aonly = Aonly

lenmult :: Trans Aonly Int
lenmult Aonly = [(1,2),(2,3),(3,1),(4,5),(5,4)]

lenmultnfa :: Nfa Aonly Int
lenmultnfa = Nfa [1,2,3,4,5] [Aonly] lenmult [1,4] [1,4]


----------------------------------------------------------------------

inclasst :: Trans Char Int
inclasst 'a' = [(0,1),(0,4),(4,3),(2,3)]
inclasst 'b' = [(1,0),(1,1),(1,2),(2,2),(2,4),(4,4),(1,4)]

inclass :: Nfa Char Int
inclass = Nfa [0,1,2,3,4] "ab" inclasst [0] [2]

-----------------------------------------------
smallert :: Trans AB Bool
smallert A = [(True,True),(True,False)]
smallert B = [(True,True)]

smaller :: Nfa AB Bool
smaller = Nfa [True,False] [A,B] smallert [True] [False]