{- code for implementing deterministic finite automata (DFA) -}
module HaskAut.FiniteAutomata.Dfa where

import HaskAut.Common.GvShow
import qualified Data.List as List


{- the type for transition functions: given a starting state and an input,
   return the next state -}
type Trans input state = (state -> input -> state)



{- representing a DFA whose input alphabet is given by *input*, and
   whose state space is given by *state* -}
data Dfa input state =
  Dfa { 
    states :: [state], -- Q: all states.  Code later assumes this is the set of reachable states
    inputs :: [input], -- Sigma: all possible inputs.  
    trans :: (Trans input state), -- delta, the transition function
    start :: state, -- s, start state
    final :: [state]} -- F, final states 




{- given a DFA, return the multistep transition function,
   which returns the state you reach when processing a sequence of
   inputs from a given starting state -}
multistep :: Dfa input state -> Trans [input] {- Sigma* -} state
multistep (Dfa _ _ delta _ _) = deltahat
  where deltahat q [] = q
        deltahat q (c:cs) = deltahat (delta q c) cs

-- an alternative definition using the foldl function
multistep' :: Dfa input state -> Trans [input] state
multistep' (Dfa _ _ delta _ _) = foldl delta




accepts :: Eq state => Dfa input state -> [input] -> Bool
accepts d@(Dfa _ _ trans start final) str =
  List.elem (multistep d start str) final

----------------------------------------------------------------------
-- showing a DFA (converting it to a string)

instance (Show input , Show state) => Show (Dfa input state) where
  show (Dfa states alphabet trans start final) =
    "States: " ++ show states ++ "\n" ++
    "Alphabet: " ++ show alphabet ++ "\n" ++
    "Transitions:\n" ++
       concat (map (\ (s,a) ->
                      "  " ++ show s ++ "," ++ show a ++ " -> " ++
                      show (trans s a) ++ "\n")
                [(s,a) | s <- states, a <- alphabet]) ++ 
    "Start state: " ++ show start ++ "\n" ++
    "Final states: " ++ show final



----------------------------------------------------------------------
-- runs: these are sequences alternating between states and inputs,
-- which show how the automata operates on an input string

data Run input state = Run [(state,input)] state

instance (Show input , Show state) => Show (Run input state) where
  show (Run r f) =
    foldr (\ (st,c) str -> show st ++ " --" ++ show c ++ "--> " ++ str) (show f) r

runh :: Trans input state -> state -> [input] -> Run input state
runh trans st [] = Run [] st
runh trans st (c:cs) =
  let (Run r f) = runh trans (trans st c) cs in
    Run ((st,c) : r) f

run :: Dfa input state -> [input] -> Run input state
run (Dfa _ _ trans start _) cs = runh trans start cs

----------------------------------------------------------------------
-- to GraphViz format

-- if printNodeNames is False, then we just draw points for nonfinal states
-- and double circles for final states.
-- 
-- To work correctly with graphviz, this function assumes that distinct states
-- and inputs are shown distinctly.
toGraphViz :: (GvShow input, GvShow state) => Bool -> Dfa input state -> String
toGraphViz printNodeNames (Dfa states inputs trans start finals) =
  let showFinal c = (foldr (\ f str -> 
                               gvshow f ++ (if printNodeNames then "" else " [label = \"\"]") ++ ";\n" ++ str)
                     c finals) in
    "digraph dfa {\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    showFinal
      ("node [shape = " ++ (if printNodeNames then "circle" else "point") ++ "];\n" ++
       "hidden -> " ++ gvshow start ++ ";\n" ++ 
       (foldr (\ st str ->
                foldr (\ c str ->
                         gvshow st ++ " -> " ++ gvshow (trans st c) ++ " [label = \"" ++ gvshow c ++ "\"];\n" ++ str)
                      str
                      inputs)
         "}\n"
         states))

writeGraphViz :: (GvShow input, GvShow state) => String -> Dfa input state -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz False d)

-- show node names
writeGraphVizN :: (GvShow input, GvShow state) => String -> Dfa input state -> IO ()
writeGraphVizN filename d =
  writeFile filename (toGraphViz True d)