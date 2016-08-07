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

-- Gets the depth of a Tree.
depth :: Tree a -> Integer
depth Leaf = -1
depth (Node n _ _ _) = n

-- Gets the number of nodes in a Tree.
nodes :: Tree a -> Integer
nodes Leaf = 0
nodes (Node _ l _ r) = 1 + nodes l + nodes r

-- Inserts a node into a Tree.
insert :: a -> Tree a -> Tree a
insert x Leaf = Node 0 Leaf x Leaf
insert x (Node _ l y r)
  | nl <= nr  = Node (depth newL + 1) newL y r
  | otherwise = Node (depth newR + 1) l y newR
  where nl = nodes l
        nr = nodes r
        newL = insert x l
        newR = insert x r

-- Generates a balanced Tree using foldr.
foldTree :: [a] -> Tree a
foldTree = foldr insert Leaf

-- Exercise 3 -------------------------------------------------------

-- Returns True only if there are an odd number of True values.
-- (Must use a fold.)
xor :: [Bool] -> Bool
xor = foldr (\x y -> x /= y) False . filter (==True)

-- Behaves identically to standard map function.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x zs -> f x : zs) []

-- OPTIONAL
-- Behaves identically to standard foldl function.
-- myFoldl :: (a -> b -> a) -> a -> [b] -> a
-- myFoldl f base xs = foldr (\x z -> ) base ()
