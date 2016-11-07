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
moreFun = max

-- Exercise 2 -------------------------------------------------------

-- Fold for Data.Tree.
treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node x ts) = f x (map (treeFold f) ts)

-- Exercise 3 -------------------------------------------------------

-- Given a boss and list of results under the boss, computes the best
-- list with and without the boss.
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss ls = (with, without)
  where with    = glCons boss . foldr mappend mempty . map snd $ ls
        without = foldr mappend mempty . map fst $ ls

-- Exercise 4 -------------------------------------------------------

-- Finds the most fun GuestList given an Employee hierarchy.
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel
