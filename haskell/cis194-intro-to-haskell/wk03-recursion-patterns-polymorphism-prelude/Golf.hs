{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Exercise 1 -------------------------------------------------------

-- Gets every x from a list of (x, m) pairs where mod m n == 0.
skip :: [(a, Int)] -> Int -> [a]
skip xs n
  | n < 1     = []
  | otherwise = [x | (x, m) <- xs, mod m n == 0]

-- Gets lists of every nth element from 1 to length.
skips :: [a] -> [[a]]
skips [] = []
skips xs = map (skip (zip xs [1..length xs])) [1..length xs]

{- Explanatory Comment:

   First, the skip function:
     The skip function takes a list of pairs (x,m) and an Int n,
     filters that list using list comprehension to only include pairs
     where m is divisible by n, then uses pattern matching in the
     list comprehension to output a list of only x's.
   The skips function:
     The skips function maps the skip function, giving it a list of
     xs zipped with 1 to length xs, over a list of Ints 1 to length
     xs. The result is that for the Ints 1 to length xs, representing
     each element in xs, the skip function is applied to the list of
     pairs (x, m), where each x corresponds to the element in xs and
     each m corresponds to the element's index + 1. The first
     argument to skip remains the same and the second increments.
     Each iteration of map produces a list of xs, with the end result
     being a list of lists of xs. -}

-- Exercise 2 -------------------------------------------------------

-- Finds all local maxima and returns them in order.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:zs) = [b | (a,b,c) <- (zip3 (x:y:zs) (y:zs) zs), a < b && b > c]
localMaxima _        = []

{- Explanatory Comment:

   Use zip3 with the full list, and then the list minus the head, and
   then that list minus the head to get only triples from the list.
   Then use list comprehension and pattern-matching to compare the
   middle Integer from the triple to its neighbors. If it's greater
   than its neighbors, include it in the output list. -}

-- Exercise 3 -------------------------------------------------------

-- Takes a list from 0 to 9 and outputs a vertical histogram.
-- histogram :: [Integer] -> String
