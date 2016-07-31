{-# OPTIONS_GHC -Wall #-}
module Golf where

import Data.List

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

-- Gets elements from a list that are equal to a given element.
getFrom :: Eq a => [a] -> a -> [a]
getFrom xs n = filter ((==) n) xs

-- Plots a single point depending on its presence in a list.
plotPoint :: Eq a => [a] -> a -> String
plotPoint xs n
  | any ((==) n) xs = "*"
  | otherwise       = " "

-- Plots a line of points from 0 to 9.
plotLine :: [Integer] -> String
plotLine xs = concatMap (plotPoint xs) [0..9] ++ "\n"

-- Takes a list from 0 to 9 and outputs a vertical histogram.
histogram :: [Integer] -> String
histogram xs = concatMap plotLine (reverse (transpose (map (getFrom xs) [0..9]))) ++ "==========\n0123456789\n"

{- Explanatory Comment:

   The getFrom function is mapped over the Ints 0 to 9, with xs as
   its first argument. This yields a list of lists, the first of
   which is a list of all 0s in xs, and the last of which is a list
   of all 9s in xs. This list of lists is then passed to transpose,
   which yields a new list of lists, the first of which is a list of
   all elements that occur at least once in the previous list, and
   the last of which is all elements that occur the maximum number of
   times (compared to any other element) in the previous list.

   At this point we have a list of lists where the first list
   contains all elements of the original list xs that occur at least
   n times, where n is equal to the largest number of duplicates
   existing in xs + 1, and the last list contains all elements of the
   original list xs that occur at least 1 time (that is, every
   unique element in xs).

   This list is then reversed and passed as the second argument to
   concatMap, where the first argument is the plotLine function. This
   yields the results of each call to plotLine, Strings, concatenated
   together into a single String, which is then concatenated to the
   constant base of the histogram.

   Regarding the plotLine function:
     The plotLine function takes an Integer list representing a row
     of data from the histogram, and uses concatMap and the plotPoint
     function along with the list of Ints 0 to 9 to produce a string
     with 10 characters. In the case of each character, the plotPoint
     function receives the Integer list representing data from the
     histogram and an Integer representing the column corresponding
     to a single piece of data. If the piece of data is present in
     the list, plotPoint outputs an asterisk, otherwise it outputs
     whitespace.

     The results of the concatMap call with plotPoint, the data row,
     and [0..9] are then themselves concatenated onto a string
     representing a newline. -}
