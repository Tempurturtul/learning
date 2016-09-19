{-# OPTIONS_GHC -Wall #-}
module Fibonacci where

-- Exercise 1 -------------------------------------------------------

-- Computes the specified Fibonacci number.
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = (+) (fib $ n-1) (fib $ n-2)

-- Defines the infinite list of Fibonacci numbers.
fibs1 :: [Integer]
fibs1 = map fib [0..]

-- Exercise 2 -------------------------------------------------------

-- More efficient version of fib.
{- If n > 1, uses an infinite [[Integer]] to determine the Fibonacci
   number, where each [Integer] contains all found Fibonacci numbers
   plus the sum of the last two. -}
fib2 :: Int -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = last $ fibs !! (n - 1)
       where fibs = iterate (\xs -> xs ++ [last xs + xs !! (length xs - 2)]) [0,1]

-- More efficient version of fibs1.
fibs2 :: [Integer]
fibs2 = map fib2 [0..]
