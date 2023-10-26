module HaskAut.Grammars.PdaExamples(module HaskAut.Grammars.Pda , module HaskAut.Grammars.PdaExamples) where

import HaskAut.Grammars.Pda

data BP = Start | Step
  deriving (Eq,Ord,Show)

ab :: Pda Char Int BP
ab = Pda [0..3] ['a','b'] [Start,Step] trans eps 0 [3]
  where eps = [(0,Push Start,1), (1,Noop,2), (2,Pop Start,3)]
        trans 'a' = [(1,Push Step,1)]
        trans 'b' = [(2,Pop Step,2)]

test = multistep ab "aaabbb"

balparens :: Pda Char Int BP
balparens = Pda [0..2] ['(',')'] [Start,Step] trans eps 0 [2]
  where eps = [(0,Push Start,1), (1,Pop Start,2)]
        trans '(' = [(1,Push Step,1)]
        trans ')' = [(1,Pop Step,1)]

test2 = accepts balparens "(()(()))"

data PaliStackSym = PChar Char | PS | Bottom
  deriving (Eq,Ord)

instance Show PaliStackSym where
  show (PChar c) = [c]
  show PS = "S"
  show Bottom = "!"

palindromes :: Pda Char Int PaliStackSym
palindromes =
  Pda
    [0,1,2,3,4,5,6,7,8,9]
    ['a','b']
    [PChar 'a', PChar 'b', PS, Bottom]
    trans eps 0 [3]
  where
    eps = [(0,Push Bottom,1),(1,Push PS,2),(2,Pop Bottom,3),

           -- S -> 
           (2,Pop PS,2),

           -- S -> a S a
           (2,Pop PS,4),(4,Push (PChar 'a'),5),(5,Push PS,6),
           (6,Push(PChar 'a'),2),

           -- S -> b S b
           (2,Pop PS,7),(7,Push (PChar 'b'),8),(8,Push PS,9),
           (9,Push(PChar 'b'),2)]
    trans 'a' = [(2,Pop (PChar 'a'),2)]
    trans 'b' = [(2,Pop (PChar 'b'),2)]


    
r 'a' = [(2,Push 'A',2),(2,Pop 'B',2)]
r 'b' = [(2,Push 'B',2),(2,Pop 'A',2)]

e = [(1,Push 'Z',2),(2,Pop 'Z',3)]

prob = Pda [1,2,3] ['a','b'] ['A','B','Z'] r e 1 [3]
