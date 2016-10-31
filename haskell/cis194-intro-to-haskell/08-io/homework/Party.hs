{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

-- Exercise 1 -------------------------------------------------------

-- Adds an Employee to the GuestList, and updates the Fun score by
-- simply adding the Employee's Fun score to the total.
glCons :: Employee -> GuestList -> GuestList
glCons e (GL l n) = GL k m
  where k = (e:l)
        m = n + empFun e

-- Monoid instance for GuestList.
instance Monoid GuestList where
  mempty                      = GL [] 0
  mappend (GL xs n) (GL ys m) = GL (xs ++ ys) (n + m)

-- Returns the more fun GuestList (either if equal).
moreFun :: GuestList -> GuestList -> GuestList
moreFun a b
  | a >= b    = a
  | otherwise = b

-- Exercise 2 -------------------------------------------------------

-- Fold for Data.Tree.
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

-- Exercise 3 -------------------------------------------------------

-- Given a boss and list of results under the boss, computes the best
-- list with and without the boss.
-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)

{-

So...

Given:
  Node (Emp "A" 1) []       *
Expect:
  (
    GL [Emp "A" 1] 1,
    GL [] 0
  )

Given:
  Node (Emp "A" 1)          *
  [
    Node (Emp "a" 1) [],    **
    Node (Emp "b" 1) []     **
  ]
Expect:
  (
    GL [Emp "A" 1] 1,
    GL [Emp "a" 1, Emp "b" 1] 2
  )

Given:
  Node (Emp "A" 1)          *
  [
    Node (Emp "a" 1) [],    **
    Node (Emp "b" 1) [],    **
    Node (Emp "c" 1)        **
    [
      Node (Emp "Z" 1) [],  *
      Node (Emp "Y" 1) []   *
    ]
  ]
Expect:
  (
    GL [Emp "A" 1, Emp "Z" 1, Emp "Y" 1] 3,
    GL [Emp "a" 1, Emp "b" 1, Emp "c" 1] 3
  )

-}

-- Exercise 4 -------------------------------------------------------

-- Finds the most fun GuestList given an Employee hierarchy.
-- maxFun :: Tree Employee -> GuestList
