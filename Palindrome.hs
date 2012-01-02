module Palindrome where
-- an adaptation of Johan Jeuring's algorithm for finding maximal palindrome lengths
-- (http://johanjeuring.blogspot.com/2007/08/finding-palindromes.html)

import Control.Arrow ((***))

-- find the maximal lengths of the palindromes
-- centered before, on, and after each element in the list
maximalPalindromeLengths :: Eq a => [a] -> [Int]
maximalPalindromeLengths as = grow 0 [] as as []
  where -- grow n lz rz as log
        --    n     - confirmed length of palindrome centered at the current position
        --    lz rz - zipper for elements at the start of the palindrome
        --    as    - elements after the palindrome
        --    log   - reversed list of maximal palindrome lengths found so far
        --
        -- The log is kept to exploit the mirror property of palindromes:
        -- any item contained in a palindrome that occurs to the left of the
        -- center reoccurs in the mirror position to the right of the center.
        --
        -- This is also true for any other palindrome whose center overlaps
        -- the first palindrome - they have a matching palindrome that mirrors
        -- them on the other side of the first's center (though the palindrome
        -- may be extended or truncated if it is not wholey contained in the
        -- first).
        grow :: Eq a => Int -> [a] -> [a] -> [a] -> [Int] -> [Int]
        -- if the items bordering the palindrome match,
        -- extend the palindrome to include those items and continue
        grow n (a':lz) rz (a:as) log | (a' == a) = grow (n+2) lz (a':rz) as log
        -- if we've reached the end of the list
        -- replay as much as the log as possible, trimming 
        -- lengths that would run over
        grow n lz rz [] log = n : zipWith min log [n-1,n-2..0]
        -- if we can't expand an empty palindrome, 
        -- try growing the next non-empty palindrome
        grow 0 lz rz as log = 0 : grow 1 lz rz (tail as) (0:log)
        -- if we can't expand a non-empty palindrome,
        -- any preceeding palindromes that are wholey contained within
        -- this one are repeated (in reverse order) on the other side of the center
        -- while the boarding one is the kernel for the next to grow
        grow n lz rz as log = n : 
            let (replay, n') = (map fst *** (uncurry min . head)) . 
                               break (uncurry (>=)) $ 
                               zip log [n-1,n-2..0]
                (xs,ys) = splitAt (n - n') rz
            in replay ++ grow n' (reverse xs ++ lz) ys as (reverse replay ++ n : log)
