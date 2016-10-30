{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Employee

-- Exercise 1 -------------------------------------------------------

-- Adds an Employee to the GuestList, and updates the Fun score by
-- simply adding the Employee's Fun score to the total.
glCons :: Employee -> GuestList -> GuestList
glCons (Emp name fun) (GL list total) = GL (list ++ [(Emp name fun)]) (total + fun)

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
