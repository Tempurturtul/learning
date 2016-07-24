{-# OPTIONS_GHC -Wall #-}
module Golf where

-- Exercise 1 -------------------------------------------------------

-- Gets lists of every nth element from n to length.
skipsFrom :: Int -> [a] -> [[a]]
skipsFrom n xs
  | n < 1 || n > length xs = []
  | otherwise              = [x | (x,i) <- (zip xs [1..length xs]), mod i n == 0] : skipsFrom (n+1) xs

-- Gets lists of every nth element from 1 to length.
skips :: [a] -> [[a]]
skips xs = skipsFrom 1 xs

{- Explanatory Comment:

    The skips function simply calls skipsFrom with an n of 1 and xs
    of the unaltered list.

    The skipsFrom function handles the case of n < 1 and the base
    case of n > length xs by returning an empty list. For all other
    cases, it forms a new list from xs by zipping each element x with
    an Int m representing the element's index in xs + 1, then uses
    list comprehension to filter the list to include only elements
    where mod m n == 0 (that is, the mth element is divisible by n),
    and then uses pattern-matching to extract x from the (x, m) pair.

    This produces a list of every nth element from xs, which is then
    cons'd onto the result of the recursive call to skipsFrom with an
    incremented n and the original xs. -}

-- Exercise 2 -------------------------------------------------------

-- Finds all local maxima and returns them in order.
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:zs)
  | y > x && y > z = y : localMaxima (y:z:zs)
  | otherwise      = localMaxima (y:z:zs)
localMaxima _ = []

{- Explanatory Comment:

     If there are at least 3 Integers in the list:
       If the 2nd Integer is greater than the 1st and 3rd:
         Cons the 2nd Integer onto the result of the recursive call
         using the list formed from the 2nd to the nth Integer.
       If the 2nd Integer isn't greater than the 1st and 3rd:
         Just return the result of the recursive call using the List
         formed from the 2nd to the nth Integer.
     If there aren't at least 3 Integers in the list:
       Just return an empty list. -}

-- Exercise 3 -------------------------------------------------------

-- Takes a list from 0 to 9 and outputs a vertical histogram.
-- histogram :: [Integer] -> String
