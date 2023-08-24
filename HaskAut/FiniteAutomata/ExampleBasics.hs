module HaskAut.FiniteAutomata.ExampleBasics where

import HaskAut.Common.GvShow
import HaskAut.FiniteAutomata.Dfa

-----------------------------------------------------
-- some datatypes for state spaces and alphabets
-- for the examples below

data ThreeStates = Q1 | Q2 | Q3
  deriving (Show , Eq , Ord , Enum)

data FourStates = S0 | S1 | S2 | S3
  deriving ( Show , Eq , Ord, Enum)

data AB = A | B
  deriving (Show , Eq , Ord , Enum)

data ABC = A_ | B_ | C_
  deriving (Show , Eq , Ord , Enum)

instance GvShow ThreeStates where
  gvshow = show

instance GvShow FourStates where
  gvshow = show

instance GvShow AB where
  gvshow = show

instance GvShow ABC where
  gvshow A_ = "A"
  gvshow B_ = "B"  
  gvshow C_ = "C"
