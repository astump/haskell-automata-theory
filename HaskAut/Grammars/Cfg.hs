{- a simple version of CFGs, where nonterminals and terminals
   both have type String. -}
module HaskAut.Grammars.Cfg (module HaskAut.Grammars.Cfg, module HaskAut.Grammars.ProdTree) where

import Data.List
import qualified Data.Map as M
import HaskAut.Common.Util
import HaskAut.Grammars.ProdTree

data Grammar =
  Grammar String {- start symbol -}
          [Prod] {- productions -}
          (String -> Bool) {- returns True for Strings which are nonterminals -} 

getTerminals :: Grammar -> [String]
getTerminals (Grammar _ ps nt) =
  nub $ filter (not . nt) $ concat $ map (\ (lhs,rhs) -> lhs:rhs) ps

grammarNt (_,_,nt) = nt

{- check that the start symbol and all left-hand sides of productions are indeed non-terminals -}
legalGrammar :: Grammar -> Bool
legalGrammar (Grammar s ps nt) = nt s && all (nt . fst) ps

{- check if a ProdTree is truly a legal derivation of a given list of symbols for a given Grammar.

   1. Check that the ProdTree's root is labeled with the start symbol.
   2. Check that the list of leaves of the ProdTree indeed is the given list of symbols
   3. Check that the productions used are contained in the grammar's productions -}
legalProdTree :: Grammar -> ProdTree -> [String] -> Bool
legalProdTree g@(Grammar s ps nt) t ss =
  (rootLabel t == s) &&
  (leaves nt t == ss) &&
  (canonOrd (computeProductions nt t) `isSubsequenceOf` canonOrd ps)

instance Show Grammar where
  show (Grammar s ps nt) =
    "Start symbol: " ++ s ++ "\n" ++
    "Productions:\n" ++
    foldr (\ (lhs,rhs) str ->
              "  " ++ lhs ++ " -> " ++ concat (intersperse " " rhs) ++ "\n" ++ str)
    "" ps


-- nonemptiness map
type Nemap = M.Map String ()

updateNonempty :: Grammar -> Nemap -> Nemap
updateNonempty (Grammar _ ps _) m =
  foldr (\ (lhs,rhs) m ->
           if (all (\ k -> M.member k m) rhs) then
             M.insert lhs () m
           else
             m) m ps

markTerminals :: Grammar -> Nemap -> Nemap
markTerminals g m = foldr (\ t m -> M.insert t () m) m (getTerminals g)

computeNemap :: Grammar -> Nemap
computeNemap g = fixedPoint (markTerminals g M.empty) (updateNonempty g)

emptyLang :: Grammar -> Bool
emptyLang g@(Grammar s _ _) =
  not $ M.member s $ computeNemap g
