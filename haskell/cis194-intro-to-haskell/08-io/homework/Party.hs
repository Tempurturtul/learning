{-# OPTIONS_GHC -Wall -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree

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

-- Fold for Data.Tree.
-- treeFold :: (b -> a -> b -> b) -> b -> Tree a -> b
-- treeFold f z (Node x []) = f z x ?

exTree01 :: Tree String
exTree01
  = Node "Hi"
    [ Node "there"
      [ Node "Bob." [],
        Node "George." []
      ],
      Node "Mister." [],
      Node "Ma'am." []
    ]

-- Exercise 3 -------------------------------------------------------
