module HaskAut.TuringMachines.TM where

import Data.List
import HaskAut.Common.GvShow
import HaskAut.Common.Util

data Direction = GoLeft | GoRight

instance Show Direction where
  show GoLeft = " \\ "
  show GoRight = " / "

data Trans state tape = Trans state tape Direction state tape 

instance (Show state , Show tape) => Show (Trans state tape) where
  show (Trans s1 t1 d s2 t2) = show s1 ++ " === " ++ show t1 ++ show d ++ show t2 ++ " " ++ " ===> " ++ show s2

data TM input state tape = TM {
  states :: [state], -- Q: all states.  
  inputs :: [input], -- Sigma: all possible inputs.
  tapesyms :: [tape], -- Gamma: all possible stack symbols.
  totape :: input -> tape, -- witnesses the inclusion of Sigma into Gamma
  blank :: tape, -- blank symbol
  leftend :: tape, -- left endmarker
  trans :: [Trans state tape], -- R: transition relation
  start :: state, -- start state
  final :: [state]} -- final states

showListIndent :: Show a => [a] -> String
showListIndent xs = concat $ intersperse "\n" $ fmap (\ s -> "  " ++ show s) xs 

instance (Show input , Show state , Show tape) => Show (TM input state tape) where
  show (TM states inputs tapesyms _ blank leftend trans start final) =
    "States: " ++ show states ++ "\n" ++
    "Alphabet: " ++ show inputs ++ "\n" ++
    "Tape symbols: " ++ show tapesyms ++ "\n" ++    
    "Blank: " ++ show blank ++ "\n" ++
    "Leftend: " ++ show leftend ++ "\n" ++
    "Transitions:\n" ++ showListIndent trans ++ "\n" ++
    "Start state: " ++ show start ++ "\n" ++
    "Final states: " ++ show final

data Config state tape = Config state tape {- the blank -} [tape] [tape]

instance (Show state, Show tape, Eq tape) => Show (Config state tape) where
  show (Config st blank leftrev right) =
    "[" ++ show st ++ ": " ++ (show $ reverse leftrev) ++ " " ++ (show $ takeWhile (/= blank) right) ++ "]"

data Configs state tape = Configs [Config state tape]

instance (Show state, Show tape, Eq tape) => Show (Configs state tape) where
  show (Configs cs) = intercalate "  " $ (show <$> cs)

data Run state tape = Run [Configs state tape]

instance (Show state, Show tape, Eq tape) => Show (Run state tape) where
  show (Run cs) = intercalate "\n" $ (show <$> cs)

showLatex :: (Show state, Show tape, Eq tape) => (Config state tape) -> String 
showLatex (Config st blank (h:leftrev) right) =
 "\\cnfg{" ++ (show $ reverse leftrev) ++ "}{" ++ show h ++ "}{" ++ (show $ takeWhile (/= blank) right) ++ "}{" ++ show st ++ "}"

shiftConfig :: Config state tape -> Direction -> Config state tape
shiftConfig (Config st blank leftrev (r:right)) GoRight = Config st blank (r:leftrev) right
shiftConfig (Config st blank (l:leftrev) (right)) GoLeft = Config st blank leftrev (l:right)

updateConfig :: Config state tape -> state -> tape -> Direction -> Config state tape 
updateConfig (Config st blank (l:leftrev) right) st' g = shiftConfig (Config st' blank (g:leftrev) right)

newConfigs :: (Eq state, Eq tape) => TM input state tape -> Config state tape -> [Config state tape]
newConfigs tm c@(Config st _ (l:leftrev) (right)) =
  let nexts = [ (st'',g'',d) | Trans st' g' d st'' g'' <- (trans tm) , st == st' , l == g' ] in
    let newConfig (st,g,d) = updateConfig c st g d in
      map newConfig nexts

-- breadth-first search for accepting config, which is returned
acceptsh :: (Eq state, Eq tape) => TM input state tape -> [Config state tape] -> Maybe (Config state tape)
acceptsh tm [] = Nothing
acceptsh tm (c@(Config st _ _ _) : cs) =
  if elem st (final tm) then Just c
  else
    acceptsh tm (cs ++ newConfigs tm c)

initialConfig :: TM input state tape -> [input] -> Config state tape
initialConfig tm xs =
  let t = map (totape tm) xs ++ repeat (blank tm) in
    Config (start tm) (blank tm) [head t , leftend tm] (tail t)

finalConfig :: (Eq state, Eq tape) => TM input state tape -> [input] -> Maybe (Config state tape)
finalConfig tm xs = acceptsh tm [initialConfig tm xs]

accepts :: (Eq state, Eq tape) => TM input state tape -> [input] -> Bool
accepts tm xs =
  case acceptsh tm [initialConfig tm xs] of
    Just _ -> True
    Nothing -> False

stepConfigs :: (Eq state, Eq tape) => TM input state tape -> Int -> [input] -> [Config state tape]
stepConfigs tm n xs = loop n [initialConfig tm xs]
  where loop n [] = []
        loop n (c : cs) | n <= 0 = (c : cs)
                        | otherwise = loop (n - 1) (cs ++ newConfigs tm c)

run :: (Eq state, Eq tape) => TM input state tape -> Int -> [input] -> Run state tape
run tm n xs = h n [initialConfig tm xs] []
  where h n [] r = Run (reverse r)
        h n (c : cs) r | n <= 0 = Run (reverse ((Configs (c : cs)) : r))
                       | otherwise = h (n - 1) (cs ++ newConfigs tm c) ((Configs (c : cs)) : r)


-- transition from p to q with no change to the tape or readhead position
epsEdge :: state -> state -> state -> [tape] -> [Trans state tape]
epsEdge p intermediate q ts =
  concat (do
             t <- ts
             return [Trans p t GoRight intermediate t, Trans intermediate t GoLeft q t])

-- a transition for moving right the read-head right, going from state st to st', and
-- updating the tape cell from g to g'
goRight :: state -> tape -> tape -> state -> [Trans state tape]
goRight st g g' st' = [Trans st g GoRight st' g']

goLeft :: state -> tape -> tape -> state -> [Trans state tape]
goLeft st g g' st' = [Trans st g GoLeft st' g']

-- if the current cell has the given value g, then transition from state p to q, moving readhead right
checkRight :: state -> tape -> state -> [Trans state tape]
checkRight p g q = goRight p g g q 

-- similar to checkRight, but move readhead left
checkLeft :: state -> tape -> state -> [Trans state tape]
checkLeft p g q = goLeft p g g q 

-- create transitions looping at the same state and leaving the current tape cell the same,
-- for the cells listed in the given [tape]
loop :: Direction -> state -> [tape] -> [Trans state tape]
loop d st = map (\ g -> Trans st g d st g) 

loopRight :: state -> [tape] -> [Trans state tape]
loopRight = loop GoRight

loopLeft :: state -> [tape] -> [Trans state tape]
loopLeft = loop GoLeft

toGraphViz :: (GvShow input, GvShow state, Eq state, GvShow tape) => Bool -> TM input state tape -> String
toGraphViz printNodeNames (TM states inputs _ _ _ _ trans start finals) =
    "digraph pda {\n" ++
    "graph [pad=\"1\", nodesep=\".5\", ranksep=\"1\"];\n" ++
    "rankdir = LR;\n" ++
    "hidden [shape = plaintext, label = \"\"];\n" ++
    "node [shape = doublecircle];\n" ++
    (foldrGlue (\ f str -> 
                   gvshow f ++ (if printNodeNames then "" else " [label = \"\"]") ++ ";\n" ++ str)
               finals
       ("node [shape = "  ++ (if printNodeNames then "circle" else "point") ++ "];\n" ++
        "hidden -> " ++ gvshow start ++ ";\n" ++ 
       -- loop over transitions st
       (foldrGlue (\ (Trans st g d st' g') str ->
                     gvshow st ++ " -> " ++ gvshow st' ++ " [label = \"" ++ gvshow g ++ (case d of { GoRight -> "/" ; GoLeft -> "\\\\"})
                     ++ gvshow g' ++ "\"];\n" ++ str)
         trans
         "}\n")))

-- write the given NFA in GraphViz format to the file named filename.
writeGraphViz :: (GvShow input, GvShow state, Eq state, GvShow tape) => String -> TM input state tape -> IO ()
writeGraphViz filename d =
  writeFile filename (toGraphViz False d)

-- same, but write node names
writeGraphVizN :: (GvShow input, GvShow state, Eq state, GvShow tape) => String -> TM input state tape -> IO ()
writeGraphVizN filename d =
  writeFile filename (toGraphViz True d)
