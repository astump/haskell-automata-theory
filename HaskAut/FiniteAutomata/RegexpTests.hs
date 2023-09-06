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

data ABC = A | B | C
  deriving (Show , Eq , Ord, Enum)

instance GvShow ABC where
  gvshow = show

e1 = Star (Concat (Char A) (Char B))

n1 = toNfa e1

-- A* + A*
e2 = Or (Star (Char A)) (Star (Char A))

n2 = toNfa e2

e3 = Or (Concat (Char A) (Star (Char B)))
        (Concat (Char C) (Star (Char B)))

n3 = toNfa e3

-- (A + BC)*
e4 = Star (Or (Char A) (Concat (Char B) (Char C)))

n4 = toNfa e4

d4 = determinize n4

du4 = dropUnreachable d4

c4 = coarsest du4

m4 = minimize du4