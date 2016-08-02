{-# OPTIONS_GHC -Wall #-}
module Homework where

-- Exercise 1 -------------------------------------------------------

{- Reimplement following functions in a more idiomatic Haskell style.

  fun1 :: [Integer] -> Integer
  fun1 []       = 1
  fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

  fun2 :: Integer -> Integer
  fun2 1 = 0
  fun2 n | even n    = n + fun2 (n `div` 2)
         | otherwise = fun2 (3 * n + 1)
-}

fun1 :: [Integer] -> Integer
fun1 = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 = sum . takeWhile (/= 1) . iterate (\n -> if even n
                                               then n `div` 2
                                               else 3 * n + 1)

-- Exercise 2 -------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

-- Generates a balanced binary tree using foldr.
-- foldTree :: [a] -> Tree a
-- foldTree = foldr (\x z -> ) Leaf
