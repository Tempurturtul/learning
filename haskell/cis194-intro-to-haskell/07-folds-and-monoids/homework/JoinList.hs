{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

{- The m parameter tracks monoidal annotations to the structure.
   Idea is that the annotation at the root of a JoinList will always
   by equal to the combination of all annotations.

   Example:
    Append (Product 210)
      (Append (Product 30)
        (Single (Product 5) 'y')
        (Append (Product 6)
          (Single (Product 2) 'e')
          (Single (Product 3) 'a')))
      (Single (Product 7) 'h') -}

-- Exercise 1 -------------------------------------------------------

-- Gets the annotation at the root of a JoinList.
tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- Appends two JoinLists (and updates monoidal annotation).
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty r = r
(+++) l Empty = l
(+++) l r     = Append (mappend (tag l) (tag r)) l r

-- Exercise 2 -------------------------------------------------------

{- Example JoinList:
     Append (Size 4)
       (Append (Size 3)
         (Single (Size 1) 'y')
         (Append (Size 2)
           (Single (Size 1) 'e')
           (Single (Size 1) 'a')))
       (Single (Size 1) 'h') -}

-- Finds the JoinList element at the specified index.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ i (Append b l r)
  | i >= s    = Nothing
  | i < ls    = indexJ i l
  | otherwise = indexJ (i - ls) r
  where s  = getSize . size $ b
        ls = getSize . size . tag $ l
indexJ _ _ = Nothing

-- Drops the first n elements from a JoinList.
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n jl
  | n <= 0 = jl
dropJ n (Append b l r)
  | n >= s    = Empty
  | n >= ls   = dropJ (n - ls) r
  | otherwise = dropJ n l +++ r
  where s  = getSize . size $ b
        ls = getSize . size . tag $ l
dropJ _ _ = Empty

-- Returns the first n elements from a JoinList.
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n (Single b a)
  | n > 0 = Single b a
takeJ n (Append b l r)
  | n >= s    = (Append b l r)
  | n >= ls   = l +++ takeJ (n - ls) r
  | otherwise = takeJ n l
  where s  = getSize . size $ b
        ls = getSize . size . tag $ l
takeJ _ _ = Empty
