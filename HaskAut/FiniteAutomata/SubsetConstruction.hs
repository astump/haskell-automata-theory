module HaskAut.FiniteAutomata.SubsetConstruction where

import HaskAut.Common.GvShow
import qualified Data.List as List
import qualified HaskAut.FiniteAutomata.Dfa as Dfa
import qualified HaskAut.FiniteAutomata.Nfa as Nfa
import HaskAut.FiniteAutomata.Nfa (Nfa(Nfa))
import HaskAut.FiniteAutomata.Dfa (Dfa(Dfa))
import Data.List
import HaskAut.Common.Util
import HaskAut.Common.Relation

sublists :: [a] -> [[a]]
sublists [] = [[]]
sublists (x:xs) =
  let r = sublists xs in
    r ++ map (x:) r

determinize :: Ord state => Nfa input state -> Dfa input [state]
determinize (Nfa states inputs trans start final) =
  Dfa
    (sublists states)
    inputs
    transD
    (canonOrd start) {- the start state of the DFA is the set of all the NFA's start states -}
    [l | l <- sublists states , not (null (List.intersect l final)) ]
  where transD ss c = image (trans c) ss