{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
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

-- Infinite list of natural numbers.
nats :: Stream Integer
nats = streamFromSeed ((+) 1) 0

-- Alternates elements from two streams.
-- (Used by ruler.)
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-- The nth element (from 1) is the largest power of 2 that evenly divides n.
-- (Pattern: zeros interleaved with ones interleaved with twos...)
ruler :: Stream Integer
ruler = incrementingInterleaved 0
      where
        incrementingInterleaved = (\n -> interleaveStreams (streamRepeat n) (incrementingInterleaved (n+1)))

-- Exercise 6 (Optional) --------------------------------------------

-- Use streams of Integers to compute Fibonacci numbers.

-- Work with generating functions of the form:
-- a_0 + a_1(t) + a_2(t^2) + ... + a_n(t^n) + ...
--   where t is just a "formal parameter" (placeholder).
--         and all the coefficients a_i are integers.

-- t = 0 + 1t + 0t^2 + 0t^3 + ...
t :: Stream Integer
t = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger = (\n -> Cons n (streamRepeat 0))
  negate      = streamMap (negate)
  (+)         = (\(Cons x xs) (Cons y ys) -> Cons (x+y) (xs + ys))
  {- Regarding multiplication:

     A*B == (a_0 + x * A')B
         == a_0 * B + x * A' * B
         == a_0(b_0 + x * B') + x * A' * B
         == a_0 * b_0 + x(a_0 * B' + A' * B)

     (N'  --> Tail of n.)
     (n_m --> n sub m.) -}
  (*)         = (\(Cons x xs) (Cons y ys) ->
                  Cons (x * y) ((streamMap ((*) x) ys) + (xs * (Cons y ys))))

instance Fractional (Stream Integer) where
  {- Regarding division:

     A/B == Q
         == (a_0 / b_0) + x((1 / b_0) * (A' - Q * B')) -}
  (/) = (\(Cons x xs) (Cons y ys) ->
          Cons
            (div x y)
            (streamMap ((*) (div 1 y)) (xs - ((Cons x xs) / (Cons y ys)) * ys)))

-- Fibonacci numbers using a generating function.
{- From instructor notes:

   F(x) = F_0 + f_1 * x + F_2 * x^2 + F_3 * x^3 + ...

   x + x * F(x) + x^2 * F(x) = F(x)

   x = F(x) - x * F(x) - x^2 * F(x)

   F(x) = x / (1 - x - x^2) -}
fibs3 :: Stream Integer
fibs3 = t / (1 - t - t^(2::Integer))
