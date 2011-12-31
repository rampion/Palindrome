module Palindrome where

import Data.Array
import Debug.Trace

input :: Array Int Char
input = listArray (0,length s - 1) s
  where s = "abacabadabacaba"

showState a n m = p ++ "(" ++ b ++ ")" ++ s
  where (p,w) = splitAt (n - m) $ elems a
        (b,s) = splitAt m w

longestPalindromes  a = extendTail afirst 0 []
  where (afirst,alast) = bounds a
        extendTail n currentTail centres = 
            trace ("extendTail     : " ++ showState a n currentTail ++ " " ++ show centres) (extendTail' n currentTail centres)
          where extendTail' n currentTail centres
                        | n > alast                    =
                            -- reached the end of the array
                            finalCentres currentTail centres (currentTail:centres)
                        | n-currentTail == afirst      =
                            -- the current longest tail palindrome
                            -- extends to the start of the array
                            extendCentres n (currentTail:centres) centres currentTail
                        | trace ("compare        : " ++ replicate (n-currentTail-1) ' ' ++ "^" ++ replicate (currentTail+2) '-' ++ "^") (a!n == a!(n-currentTail-1))   =
                            -- the current longest tail palindrome
                            -- can be extended
                            extendTail (n+1) (currentTail+2) centres
                        | otherwise                    =
                            -- the current longest tail palindrome
                            -- cannot be extended
                            extendCentres n (currentTail:centres) centres currentTail
        extendCentres n centres tcentres centreDistance = 
            trace ("extendCenteres : " ++ showState a n centreDistance ++ " " ++ show centres ++ " " ++ show tcentres) (extendCentres' n centres tcentres centreDistance)
          where extendCentres' n centres tcentres centreDistance
                    | centreDistance == 0                =
                        -- the last centre is on the last element:
                        -- try to extend the tail of length 1
                        extendTail (n+1) 1 centres
                    | centreDistance-1 == head tcentres  =
                        -- the previous element in the centre list
                        -- reaches exactly to the end of the last
                        -- tail palindrome use the mirror property
                        -- of palindromes to find the longest tail
                        -- palindrome
                        extendTail n (head tcentres) centres
                    | otherwise                          =
                        -- move the centres one step
                        -- add the length of the longest palindrome
                        -- to the centres
                        extendCentres n (min (head tcentres) (centreDistance-1):centres) (tail tcentres) (centreDistance-1)

        finalCentres n tcentres centres = 
            trace ("finalCenteres  : " ++ showState a (alast + 1) n ++ " " ++ show centres ++ " " ++ show tcentres) (finalCentres' n tcentres centres)
          where finalCentres' 0     tcentres centres  =  centres
                finalCentres' n     tcentres centres  = finalCentres (n-1) (tail tcentres) (min (head tcentres) (n-1):centres)
