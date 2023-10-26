module HaskAut.TuringMachines.TMExamples
  (module HaskAut.TuringMachines.TMExamples ,
   module HaskAut.TuringMachines.UniversalTM,
   module HaskAut.TuringMachines.TM) 
  where

import HaskAut.TuringMachines.UniversalTM
import HaskAut.TuringMachines.TM

----------------------------------------------------------------------
-- recognize {a^n b^n c^n | n in Nat }
tripletm =
  TM [1 .. 6] "abc" "abc*! " id ' ' '!' trans 1 [6]
  where
    trans = checkRight 1 ' ' 6 ++
            loopRight 1 "*" ++
            goRight 1 'a' '*' 2 ++
            loopRight 2 "a*" ++
            goRight 2 'b' '*' 3 ++
            loopRight 3 "b*" ++
            goRight 3 'c' '*' 4 ++
            loopRight 4 "c*" ++
            checkLeft 4 ' ' 5 ++
            loopLeft 5 "abc*" ++
            checkRight 5 '!' 1 

test = stepConfigs tripletm 35 "aabbcc"

----------------------------------------------------------------------
-- recognize language { ww | w in {a,b}* }
ww =
  TM [1..10] "ab" "ab*! " id ' ' '!' trans 1 [10]
  where
    trans =
      -- (1) erase first a, then move right and nondeterministically erase a matching a
      goRight 1 'a' '*' 2 ++
      loopRight 2 "ab" ++
      goLeft 2 'a' '*' 4 ++

      -- same as (1) but for letter b
      goRight 1 'b' '*' 3 ++
      loopRight 3 "ab" ++
      goLeft 3 'b' '*' 4 ++

      loopLeft 4 "ab*" ++

      checkRight 4 '!' 5 ++
      loopRight 5 "*" ++

      checkLeft 5 ' ' 10 ++

      goRight 5 'a' '*' 6 ++
      loopRight 6 "ab" ++
      checkRight 6 '*' 7 ++
      loopRight 7 "*" ++
      goLeft 7 'a' '*' 4 ++

      goRight 5 'b' '*' 8 ++
      loopRight 8 "ab" ++
      checkRight 8 '*' 9 ++
      loopRight 9 "*" ++
      goLeft 9 'b' '*' 4 

      
       


{-  TM [1..13] "abc" "abc*! " id ' ' '!' trans 1 [7]
  where
    checkABLeft p q = 
      checkLeft p 'a' q ++
      checkLeft p 'b' q
    checkABRight p q = 
      checkRight p 'a' q ++
      checkRight p 'b' q
    trans =

      -- [w2] (i) nondeterministically pick an a and erase it
      loopRight 1 "ab" ++
      goLeft 1 'a' '*' 2 ++

      -- [w2] (ii) erase a b
      goLeft 1 'b' '*' 8 ++

      ------------------------------------------
      -- the a loop
      ------------------------------------------
      
      -- [w1] move left through a's and b's only until hitting erasure or left end, then erase matching a
      loopLeft 2 "ab" ++
      checkRight 2 '*' 12 ++
      checkRight 2 '!' 12 ++ 
      goRight 12 'a' '*' 3 ++
      
      -- [w1] skip a's and b's to get to w2
      loopRight 3 "ab" ++

      -- [w2] skip erasures moving right
      checkRight 3 '*' 4 ++
      loopRight 4 "*" ++

      -- [w2] (i) erase first a encountered, then skip all the erasures moving left
      goLeft 4 'a' '*' 5 ++
      loopLeft 5 "*" ++

      -- [w1] (a) skip at least one a or b to move back to the start of the a loop
      checkABLeft 5 2 ++

      -- [w1] (b) instead nondeterministically erase an a, because otherwise you might be moving to the left of the last remaining a
      goRight 5 'a' '*' 3 ++
  
      -- [w2] (ii) if instead of an a, you see b, drop down to the b loop
      goLeft 4 'b' '*' 11 ++

      -- [w2] skip erasures
      loopLeft 11 "*" ++

      -- [w1] (a) skip at least one a or b to move back to start of the b loop
      checkABLeft 11 8 ++

      -- [w1] (b) instead nondeterministically erase a b, since otherwise you might move past the last remaining b
      goRight 11 'b' '*' 9 ++

      ------------------------------------------
      -- the b loop
      ------------------------------------------

      -- [w1] move left through a's and b's to erase a b
      loopLeft 8 "ab" ++
      checkRight 8 '*' 13 ++
      checkRight 8 '!' 13 ++
      goRight 13 'b' '*' 9 ++
      
      -- [w1] skip a's and b's to get to w2
      loopRight 9 "ab" ++

      -- [w2] skip erasures moving right
      checkRight 9 '*' 10 ++
      loopRight 10 "*" ++

      -- [w2] (i) erase first b encountered, then skip all the erasures moving left
      goLeft 10 'b' '*' 11 ++

      -- [w2] (ii) if instead of a b, you see an a, pop up to the a loop
      goLeft 10 'a' '*' 5 ++

      ---------------------------------
      -- check all erased
      ---------------------------------

      -- if we reach a blank (so after w2)
      checkLeft 4 ' ' 6 ++
      checkLeft 10 ' ' 6 ++

      -- skip all erasures, looking for the left endmarker
      loopLeft 6 "*" ++
      checkRight 6 '!' 7
-}

test2 = accepts ww "abaaba"

----------------------------------------------------------------------
data Finput = A_ | B_ | C_

instance Show Finput where
  show A_ = "a"
  show B_ = "b"
  show C_ = ":"    

data Ftape = A__ | B__ | C__ | L__ | S__
  deriving Eq

instance Show Ftape where
  show A__ = "a"
  show B__ = "b"  
  show C__ = ":"
  show L__ = "!"
  show S__ = " "    

embedFinput :: Finput -> Ftape
embedFinput A_ = A__
embedFinput B_ = B__
embedFinput C_ = C__

instance UEncode Ftape where
  bitenc A__ = "000"
  bitenc B__ = "001"
  bitenc C__ = "010"
  bitenc L__ = "011"
  bitenc S__ = "100"        

simple =
  TM [1 .. 3] [A_,B_,C_] [A__,B__,C__,L__,S__] embedFinput S__ L__ trans 1 [3]
  where
    trans = checkRight 1 A__ 1 ++
            checkRight 1 B__ 2 ++
            checkRight 2 B__ 2 ++            
            goRight 2 C__ A__ 3

            