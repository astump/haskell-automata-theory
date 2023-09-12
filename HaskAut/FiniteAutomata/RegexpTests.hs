module HaskAut.FiniteAutomata.RegexpTests
  (module HaskAut.FiniteAutomata.RegexpTests,
   module HaskAut.FiniteAutomata.Regexp,
   module HaskAut.FiniteAutomata.NfaEpsilon,
   module HaskAut.Common.GvShow,
   module HaskAut.FiniteAutomata.SubsetConstructionEps,
   module HaskAut.FiniteAutomata.DfaOperations) where

import HaskAut.FiniteAutomata.Regexp
import HaskAut.FiniteAutomata.NfaEpsilon
import HaskAut.Common.GvShow
import HaskAut.FiniteAutomata.SubsetConstructionEps
import HaskAut.FiniteAutomata.DfaOperations

e1 = Star (Concat (Char 'a') (Char 'b'))

n1 = toNfa e1

-- A* + A*
e2 = Or (Star (Char 'a')) (Star (Char 'a'))

n2 = toNfa e2

e3 = Or (Concat (Char 'a') (Star (Char 'b')))
        (Concat (Char 'c') (Star (Char 'b')))

n3 = toNfa e3

-- (A + BC)*
e4 = Star (Or (Char 'a') (Concat (Char 'b') (Char 'c')))

n4 = toNfa e4

d4 = determinize n4

du4 = dropUnreachable d4

c4 = coarsest du4

m4 = minimize du4