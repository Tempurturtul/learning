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
-- nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
-- nextLevel e ls = (with, without)
--   where with    = foldr moreFun (GL [e] (empFun e)) . map (glCons e) $ map snd ls
--         without = foldr moreFun (GL [] 0) $ map fst ls

{-

So...

Given:                            "If x is invited:
                                    can't invite *!,
                                    can invite *,
                                    can either invite *n or *m."

  Node (Emp "Stan" 9)             *     |       |       |
  [ Node (Emp "Bob" 2)            *!    |  *    |       |
    [ Node (Emp "Joe" 5)          *1    |  *!   |  *    |
      [ Node (Emp "John" 1) []    *2    |  *    |  *!   |
      , Node (Emp "Sue" 5) []     *2    |  *    |  *!   |
      ]                                 |       |--------
    , Node (Emp "Fred" 3) []      *     |  *!   |  *    |
    ]                                   |----------------
  , Node (Emp "Sarah" 17)         *!    |  *    |       |
    [ Node (Emp "Sam" 4) []       *     |  *!   |  *    |
    ]
  ]

-}

-- Exercise 4 -------------------------------------------------------

-- Finds the most fun GuestList given an Employee hierarchy.
-- maxFun :: Tree Employee -> GuestList
-- maxFun = uncurry moreFun . treeFold nextLevel
