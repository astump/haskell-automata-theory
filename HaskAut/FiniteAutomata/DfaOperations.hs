{- implements some operations on DFAs -}
module HaskAut.FiniteAutomata.DfaOperations where

import Data.List 
import HaskAut.FiniteAutomata.Dfa
import HaskAut.Common.Util

{- return a DFA whose language is the complement of the language
   of the input DFA. -}
complement :: Eq state => Dfa input state -> Dfa input state
complement (Dfa states inputs trans start final) =
  (Dfa states inputs trans start (states \\ final))

prod :: [state1] -> [state2] -> [(state1, state2)]
prod states1 states2 = [(s, s') | s <- states1 , s' <- states2]

prodTrans :: Trans input state1 -> Trans input state2 -> Trans input (state1, state2)
prodTrans trans1 trans2 (s, s') c = (trans1 s c,trans2 s' c)

union :: Dfa input state1 -> Dfa input state2 -> Dfa input (state1, state2)
union (Dfa states1 inputs trans1 start1 final1) (Dfa states2 _ trans2 start2 final2) =
  Dfa (prod states1 states2) inputs (prodTrans trans1 trans2) (start1, start2)
    ([ (s, s') | s <- final1, s' <- states2] ++ [(s, s') | s <- states1 , s' <- final2])

intersect :: Dfa input state1 -> Dfa input state2 -> Dfa input (state1, state2)
intersect (Dfa states1 inputs trans1 start1 final1) (Dfa states2 _ trans2 start2 final2) =
  Dfa (prod states1 states2) inputs (prodTrans trans1 trans2) (start1, start2) (prod final1 final2)
