{----------------------------------------------------------
  Motivation
----------------------------------------------------------}

-- Consider functions designed to "map" a function over every
-- element of some sort of container:

--    map :: (a -> b) -> [a] -> [b]
--    treeMap :: (a -> b) -> Tree a -> Tree b

-- The repeated pattern looks like so:
--    thingMap :: (a -> b) -> f a -> f b

{----------------------------------------------------------
  A Brief Digression on Kinds
----------------------------------------------------------}

-- Every expression has a type.
-- Types themselves have "types", called kinds.

-- Use :kind to check.
--    Prelude> :k Int
--    Int :: *
--    Prelude> :k Maybe Int
--    Maybe Int :: *
--    Prelude> :k Maybe
--    Maybe :: * -> *

-- The above illustrates that all types that can serve as the type of
-- some value have kind *, and that things like Maybe (there are no
-- values of type Maybe) are basically a function on types (usually
-- called a type constructor).

-- More examples:
--    Prelude> :k []
--    [] :: * -> *
--    Prelude> :k [] Int
--    [] Int :: *
--    Prelude> :k Tree
--    Tree :: * -> *

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)

--    Prelude> :k JoinList
--    JoinList :: * -> * -> *

-- The above indicates that JoinList expects two types as parameters
-- and gives back a new type. (Considering that it's curried, it's
-- more accurate to think that it takes one type and gives back * -> *.)

--    Prelude> :k (->)
--    (->) :: * -> * -> *
--    Prelude> :k Int -> Char
--    Int -> Char :: *

data Funny f a = Funny a (f a)

--    Prelude> :k Funny
--    Funny :: (* -> *) -> * -> *

-- GHCi uses kind inference (in the same way it uses type inference).

-- Note that types can be partially applied, just like functions:
--    Prelude> :k Funny Maybe
--    Funny Maybe :: * -> *
--    Prelude> :k Funny Maybe Int
--    Funny Maybe Int :: *

{----------------------------------------------------------
  Functor
----------------------------------------------------------}

-- thingMap :: (a -> b) -> f a -> f b

-- In thingMap, f is a type variable standing in for some type of
-- kind * -> *.

-- thingMap has to work differently for each f.

-- The solution is to make a type class for f, traditionally called
-- Functor.

class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- Note: The name functor comes from category theory, not the
-- unrelated C++ functors.

-- It does not make sense to define instances for types of kind *
-- because the first argument of Functor has kind * -> *.

instance Functor Maybe where
  fmap _ Nothing = Nothing
  fmap h (Just a) = Just (h a)

instance Functor [] where
  fmap _ []     = []
  fmap f (x:xs) = f x : fmap f xs
  -- or just:
  -- fmap = map

instance Functor IO where
  -- fmap f ioa = ioa >>= (\a -> return (f a))
  -- Better:
  fmap f ioa = ioa >>= (return . f)

-- Below: think about f = (->) e

instance Functor ((->) e) where
  -- fmap :: (a -> b) -> (->) e a -> (->) e b
  -- or:
  -- fmap :: (a -> b) -> (e -> a) -> (e -> b)
  -- or:
  fmap = (.)
