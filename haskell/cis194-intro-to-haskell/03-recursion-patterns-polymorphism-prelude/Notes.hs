{----------------------------------------------------------
  Recursion Patterns
----------------------------------------------------------}

-- Experienced Haskell programmers hardly ever write recursive functions.
-- Achieved by abstracting out recursive patterns into library functions.
-- Examples: Map, Filter, Reduce/Fold.

-- Recall previous simple definition of lists of Int values:
data IntList = Empty | Cons Int IntList
  deriving Show

-- Instead of the following recursive function:
absAll :: IntList -> IntList
absAll Empty = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

-- Abstract out the recursive pattern:
--   mapIntList abs exampleList

{----------------------------------------------------------
  Polymorphism
----------------------------------------------------------}

-- Polymorphic data type.
-- t is a type variable, can stand for any type.
data List t = E | C t (List t)

lst1 :: List Int
lst1 = C 3 (C 5 (C 2 E))

lst2 :: List Char
lst2 = C 'x' (C 'y' (C 'z' E))

lst3 :: List Bool
lst3 = C True (C False E)

{----------------------------------------------------------
  Polymorphic Functions
----------------------------------------------------------}

-- Filter a list.
filterList _ E = E
filterList p (C x xs)
  | p x       = C x (filterList p xs)
  | otherwise = filterList p xs

-- Using :t on above will display: (t -> Bool) -> List t -> List t
{- This indicates that filterList takes a function from t to Bool and
   a list of t's, and returns a list of t's. -}

-- Map to a list (can change type).
mapList :: (a -> b) -> List a -> List b
mapList _ E        = E
mapList f (C x xs) = C (f x) (mapList f xs)

{----------------------------------------------------------
  The Prelude
----------------------------------------------------------}

-- The Prelude is a module with standard definitions.

-- Data.List module has (additional) functions for working with lists.

-- A useful polymorphic type to be aware of is Maybe.
data Maybe a = Nothing | Just a
-- Data.Maybe module has functions for working with Maybe values.

{----------------------------------------------------------
  Total and Partial Functions
----------------------------------------------------------}

-- Consider: [a] -> a
-- Example: head
-- Crashes when list is empty.
-- This is a partial function.

{- Functions with certain inputs that will cause infinite recursion
   are also considered partial. -}

-- Total functions are well-defined on all possible inputs.

-- According to the instructor: head is a mistake.
--   As is: tail, init, last, and (!!).

-- What to do instead? Pattern-matching.
-- Consider:

doStuff1 :: [Int] -> Int
doStuff1 []  = 0
doStuff1 [_] = 0
doStuff1 xs  = head xs + (head (tail xs))

doStuff2 :: [Int] -> Int
doStuff2 []        = 0
doStuff2 [_]       = 0
doStuff2 (x1:x2:_) = x1 + x2

-- Both of the above are total.
-- The second is obviously total.

-- How to avoid writing partial functions?
-- Two options:

-- Option A: Change the output type to indicate possible failure.
--   Maybe uses option A.
--     data Maybe a = Nothing | Just a

-- Rewriting head with option A:
safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

-- The above exists in the safe package.

-- Option B: Use a different type.
-- Consider:

-- What if we "know" a list will never be empty?
--   Reflect that in the type!
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNEL :: [a] -> Maybe (NonEmptyList a)
listToNEL []     = Nothing
listToNEL (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as
