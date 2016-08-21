{----------------------------------------------------------
  Parametricity
----------------------------------------------------------}

{- Type information is erased by the compiler after being checked.
   Therefore, there is no equivalent to the `instanceof` operator. -}

{- A function like f :: a -> a -> a is said to be parametric in the
   type a.

   There are only two functions that could have type a -> a -> a:

     f1 :: a -> a -> a
     f1 x y = x

     and

     f2 :: a -> a -> a
     f2 x y = y
-}

{----------------------------------------------------------
  Two Views On Parametricity
----------------------------------------------------------}

{- Implementor's view: annoying restrictions.
   User's view: useful guarantees. -}

{----------------------------------------------------------
  Type Classes
----------------------------------------------------------}

{-
  (+)  :: Num a  => a -> a -> a
  (==) :: Eq a   => a -> a -> Bool
  (<)  :: Ord a  => a -> a -> Bool
  show :: Show a => a -> String

  Num, Eq, Ord, and Show are type classes.

  (+), (==), (<) are said to be "type-class polymorphic."

  The part before the => is a type class constraint.
-}

{- Type classes correspond to sets of types which have certain
   operations defined for them. -}

{-
  class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

  The above indicates that any type a which wants to be an
  instance of Eq must define the functions (==) and (/=) with
  the indicated type signatures.
-}

{- Type inference is used to determine which implementation of
   a type-class polymorphic function should be chosen, based on
   the types of its arguments. -}

data Foo = F Int | G Char

instance Eq Foo where
  (F i1) == (F i2) = i1 == i2
  (G c1) == (G c2) = c1 == c2
  _ == _ = False

  foo1 /= foo2 = not (foo1 == foo2)

{- Type classes can give default implementations of methods in
   terms of other methods.

   class Eq a where
     (==) :: a -> a -> Bool
     (/=) :: a -> a -> Bool
     x /= y = not (x == y)

   With the above, an instance of Eq only needs to specify an
   implementation of (==) and will get (/=) for free. (/=) can
   still be overridden though if desired.

   The actual Eq class is declared like so:

   class Eq a where
     (==), (/=) :: a -> a -> Bool
     x == y = not (x /= y)
     x /= y = not (x == y)

   The above allows the definition of either function and
   infers the other.
-}

{- Eq and some others are special in that GHC is able to
   automatically generate instances.

   data Foo' = F' Int | G' Char
     deriving (Eq, Ord, Show)

   The above automatically derives instances of Eq, Ord, and
   Show for the data type Foo'.
-}

{----------------------------------------------------------
  Type Classes and Java Interfaces
----------------------------------------------------------}

{- Type class instances are declared separately from the
   declaration of the corresponding types. -}
{- Types that can be specified for type class methods are
   more general and flexible, especially considering multi-
   paramter type classes. -}

{- Easier to do in Haskell than Java:

   Essentially multiple dispatch:
     class Blerg a b where
       blerg :: a -> b -> Bool

   Binary, ternary, ... methods:
     class Num a where
       (+) :: a -> a -> a
       ...
-}

{----------------------------------------------------------
  Standard Type Classes
----------------------------------------------------------}

{- Ord
     For types whose elements can be totally ordered.

     Provides comparison operators like (<) and (<=), and the
     compare function. -}

{- Num
     For numeric types.

     Supports things like addition, multiplication...

     Note that literals like 5 are actually type class
     polymorphic; that is, they can be used as Ints, Integers,
     Doubles, or any other type that is an instance of Num. -}

{- Show
     Defines the show method. -}

{- Read
     Dual of show. -}

{- Integral
     Whole numbers. -}

{----------------------------------------------------------
  A Type Class Example
----------------------------------------------------------}

-- Class of things which can be converted to a list of Ints.
class Listable a where
  toList :: a -> [Int]

instance Listable Int where
  toList x = [x]

instance Listable Bool where
  toList True  = [1]
  toList False = [0]


{- GHC complains about these two examples.

instance Listable [Int] where
  toList = id

data Tree a = Empty | Node a (Tree a) (Tree a)

instance Listable (Tree Int) where
  toList Empty        = []
  toList (Node x l r) = toList l ++ [x] ++ toList r

-}

sumL :: Listable a => a -> Int
sumL x = sum (toList x)

foo :: (Listable a, Ord a) => a -> a -> Bool
foo x y = sum (toList x) == sum (toList y) || x < y

instance (Listable a, Listable b) => Listable (a,b) where
  toList (x,y) = toList x ++ toList y

{- Above says that a pair type (a,b) is an instance of Listable
   if a and b are both instances of Listable.

   Note that the above is not recursive, it calls other versions
   of toList, not itself. Also note the type class contraint. -}
