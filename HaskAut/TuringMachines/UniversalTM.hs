module HaskAut.TuringMachines.UniversalTM where

import HaskAut.TuringMachines.TM
import HaskAut.Common.Util
import Data.Char

utape :: String
utape = ['0','1','#',',','.']

class UEncode a where
  bitenc :: a -> String -- over utape

bitChar :: Bool -> Char
bitChar True = '1'
bitChar False = '0'

instance UEncode Int where
  bitenc x = if x <= 0 then []
             else
               (bitChar $ x `mod` 2 == 1) : bitenc (x `div` 2)

instance UEncode Char where
  bitenc = bitenc . ord

instance UEncode Direction where
  bitenc GoLeft = "1"
  bitenc GoRight = "0"

instance UEncode Integer where
  bitenc x = bitenc (fromIntegral x :: Int)

instance (UEncode state, UEncode tape) => UEncode (Trans state tape) where
  bitenc (Trans st g d st' g') =
    dot st $
    dot g $
    dot d $
    dot g' $
    dot st' $ ""

{- convenience function to encode a to a string of 1s and 0s,
   and then append another string.  The Char u separates the
   two strings.-}
bitencSep :: UEncode a => Char -> a -> String -> String
bitencSep u x rest = bitenc x ++ (u : rest)

pound :: UEncode a => a -> String -> String
pound = bitencSep '#'
comma :: UEncode a => a -> String -> String
comma = bitencSep ','
dot :: UEncode a => a -> String -> String
dot = bitencSep '.'

list :: UEncode a => [a] -> String -> String
list xs rest = foldrGlue comma xs ('#' : rest)

encode :: (UEncode state, UEncode tape) =>
          TM input state tape -> String -- over Utape
encode (TM states inputs tapesyms _ blank leftend trans start final) =
  pound leftend $
  pound blank $
  pound start $
  list final $
  list trans ""
    
  
  
  
