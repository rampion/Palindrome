-- tests adapted from:
--  https://subversion.cs.uu.nl/repos/staff.johanj.palindromes/trunk/tests/Main.hs
--  which is (c) 2007 - 2011 Johan Jeuring, and licensed under the BSD3 license

module Main where

import Data.Array
import Data.Char

import Test.QuickCheck

import Palindrome

propMaximalPalindromeLengths :: Property
propMaximalPalindromeLengths = 
  forAll (arbitrary:: Gen [Int]) $ 
  \l -> let a = array (0,length l - 1) (zip [0..] l)
        in maximalPalindromeLengths l == longestPalindromesQ a

longestPalindromesQ    ::  Eq a => Array Int a -> [Int]
longestPalindromesQ a  =   
  let (afirst,alast)  =  bounds a
      positions       =  [0 .. 2*(alast-afirst+1)]
  in  map (lengthPalindromeAround a) positions

lengthPalindromeAround  ::  Eq a => Array Int a 
                                 -> Int 
                                 -> Int
lengthPalindromeAround a position 
  | even position = 
      extendPalindromeAround (afirst+pos-1) (afirst+pos) 
  | odd  position = 
      extendPalindromeAround (afirst+pos-1) (afirst+pos+1) 
  where  pos             =  div position 2
         (afirst,alast)  =  bounds a
         extendPalindromeAround start end  = 
            if   start < 0 
              || end > alast-afirst 
              || a!start /= a!end
            then end-start-1
            else extendPalindromeAround (start-1) (end+1) 

main = quickCheck propMaximalPalindromeLengths
