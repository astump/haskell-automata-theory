module HaskAut.FiniteAutomata.DfaExamples
  -- export the names from this and all the imported modules
  (module HaskAut.FiniteAutomata.DfaExamples,
   module Dfa, module ExampleBasics, module Util) where

import HaskAut.FiniteAutomata.Dfa as Dfa
import HaskAut.FiniteAutomata.ExampleBasics as ExampleBasics
import HaskAut.Common.Util as Util

---------------------------------------------------------
-- Example DFA from class 8/23

delta1 :: Trans AB ThreeStates
delta1 Q1 A = Q1
delta1 Q1 B = Q2
delta1 Q2 A = Q3
delta1 Q2 B = Q2
delta1 Q3 A = Q3
delta1 Q3 B = Q3

dfa1 :: Dfa AB ThreeStates
dfa1 = Dfa [Q1,Q2,Q3] [A,B] delta1 Q1 [Q2]

s1 = multistep dfa1 Q1 [A,A,A,B,B]
accepting1 = accepts dfa1 [A,A,A,B,B]
rejecting1 = accepts dfa1 [A,A]

r1 = run dfa1 [A,A,A,B,B]

-- same example, but using just Char and Int for alphabet and states, respectively
delta1' :: Trans Char Int
delta1' 1 'a' = 1
delta1' 1 'b' = 2
delta1' 2 'a' = 3
delta1' 2 'b' = 2
delta1' 3 'a' = 3
delta1' 3 'b' = 3

dfa1' :: Dfa Char Int
dfa1' = Dfa [1,2,3] "ab" delta1' 1 [2]

----------------------------------------------------------------------
-- example from game in class Thursday

delta2 :: Trans AB FourStates
delta2 S0 A = S1
delta2 S0 B = S3
delta2 S1 A = S2
delta2 S1 B = S1
delta2 S2 A = S2
delta2 S2 B = S3
delta2 S3 A = S3
delta2 S3 B = S3


dfag :: Dfa AB FourStates
dfag = Dfa [S0,S1,S2,S3] [A,B] delta2 S0 [S2]

--------------------------------------------------
{- accepting strings of the form [B,A,B,A,...] -}
baTrans :: Trans AB FourStates
baTrans S0 B = S1
baTrans S1 A = S2
baTrans S2 B = S1
baTrans _ _ = S3 -- rejecting state

dfa2 :: Dfa AB FourStates
dfa2 = Dfa [S0 ..] [A,B] baTrans S0 [S0,S2]

sba = multistep dfa2 S0 [B,A,B]
acceptingba = accepts dfa2 [B,A,B,A,B,A]
rejectingba = accepts dfa2 [B,A,B,A,A]

r2 = run dfa2 [B,A,B,A,A]

---------------------------------------------------------
-- DFA from exercise 3.2 of Kozen.  It accepts
-- exactly the strings containing three consecutive a's

transEx2 :: Trans AB FourStates
transEx2 S0 A = S1
transEx2 S0 B = S0
transEx2 S1 A = S2
transEx2 S1 B = S0
transEx2 S2 A = S3
transEx2 S2 B = S0
transEx2 S3 A = S3
transEx2 S3 B = S3

ex2 :: Dfa AB FourStates
ex2 = Dfa [S0 ..] [A,B] transEx2 S0 [S3]

s2 = multistep ex2 S0 [B,A,A,A,B]
accepting2 = accepts ex2 [B,A,A,A,B]
rejecting2 = accepts ex2 [B,A,A,B,A,B]



