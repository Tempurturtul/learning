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
   number, where each [Integer] contains the last two calculated
   Fibonacci numbers. -}
fib2 :: Int -> Integer
fib2 0 = 0
fib2 1 = 1
fib2 n = last $ fibs !! (n - 1)
       where fibs = iterate (\xs -> last xs : (head xs + last xs) : []) [0,1]

-- More efficient version of fibs1.
fibs2 :: [Integer]
fibs2 = map fib2 [0..]

-- Exercise 3 -------------------------------------------------------

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show s = show $ take 20 (streamToList s)

-- Converts a Stream to an infinite list.
streamToList :: Stream a -> [a]
streamToList (Cons x xs) = [x] ++ streamToList xs

-- Exercise 4 -------------------------------------------------------

-- Generates a Stream containing copies of the given element.
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- Applies a function to every element of a Stream.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Generates a Stream from a "seed" using an "unfolding rule" function.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- Exercise 5 -------------------------------------------------------
