{-# OPTIONS_GHC -Wall #-}
module JoinList where

import Sized
import Data.Maybe

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

-- Finds the JoinList element at the specified index.
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ i _
  | i < 0      = Nothing
indexJ _ Empty = Nothing
indexJ i (Single _ a)
  | i == 0     = Just a
  | otherwise  = Nothing
indexJ i (Append b l r)
  | i > s - 1      = Nothing
  | isJust lResult = lResult
  | otherwise      = rResult
  where s       = getSize . size $ b
        ls      = getSize . size . tag $ l
        lResult = indexJ i l
        rResult = indexJ (i - ls) r

{- Example JoinList for indexJ:
     Append (Size 4)
       (Append (Size 3)
         (Single (Size 1) 'y')
         (Append (Size 2)
           (Single (Size 1) 'e')
           (Single (Size 1) 'a')))
       (Single (Size 1) 'h') -}
