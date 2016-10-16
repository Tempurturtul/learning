{-# LANGUAGE GeneralizedNewtypeDeriving #-}

{----------------------------------------------------------
  Folds, Again
----------------------------------------------------------}

-- Binary tree.
data Tree a = Empty
            | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

-- Computes the size of a Tree.
treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

-- Sums the data in a Tree of Integers.
treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

-- Computes the depth of a Tree.
treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

-- Flattens a Tree into a list.
flatten :: Tree a -> [a]
flatten Empty        = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r

{-
  Each of the above:
    - Takes a Tree as input.
    - Pattern-matches on the input Tree.
    - Gives a simple answer in the Empty case.
    - In the Node case:
      - Calls itself recursively on both subtrees.
      - Somehow combines the results of the recursive calls with the
      data x to produce the final result.

  To abstract out the repeating patterns, the following need to be
  passed as parameters:
    - The return type.
    - The answer in the Empty case.
    - How to combine the recursive calls.
-}

-- Fold for Tree.
treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty        = e
treeFold e f (Node l x r) = f (treeFold e f l) x (treeFold e f r)

treeSize' :: Tree a -> Integer
treeSize' = treeFold 0 (\l _ r -> 1 + l + r)

treeSum' :: Tree Integer -> Integer
treeSum' = treeFold 0 (\l x r -> l + x + r)

treeDepth' :: Tree a -> Integer
treeDepth' = treeFold 0 (\l _ r -> 1 + max l r)

flatten' :: Tree a -> [a]
flatten' = treeFold [] (\l x r -> l ++ [x] ++ r)

-- Computes the max in a Tree.
treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l x r -> l `max` x `max` r)

-- Folding Expressions

-- ExprT type from Homework 5 eval function.
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT

-- eval function from Homework 5.
eval :: ExprT -> Integer
eval (Lit i)     = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Mul e1 e2) = eval e1 * eval e2

-- Fold for ExprT.
exprTFold :: (Integer -> b) -> (b -> b -> b) -> (b -> b -> b) -> ExprT -> b
exprTFold f _ _ (Lit i)     = f i
exprTFold f g h (Add e1 e2) = g (exprTFold f g h e1) (exprTFold f g h e2)
exprTFold f g h (Mul e1 e2) = h (exprTFold f g h e1) (exprTFold f g h e2)

eval2 :: ExprT -> Integer
eval2 = exprTFold id (+) (*)

-- Counts the number of literals in an expression.
numLiterals :: ExprT -> Int
numLiterals = exprTFold (const 1) (+) (+)

{----------------------------------------------------------
  Monoids
----------------------------------------------------------}

class Monoid' m where
  mempty'  :: m
  mappend' :: m -> m -> m

  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

-- Synonym for mappend'.
(<>) :: Monoid' m => m -> m -> m
(<>) = mappend'

{-
  Regarding Monoid':
  - mempty' is an identity for <>
  - <> is associative

  For example, for all x, y, and z:
    1. mempty' <> x == x
    2. x <> mempty' == x
    3. (x <> y) <> z == x <> (y <> z)
-}

-- Lists form a monoid under concatenation:
instance Monoid' [a] where
  mempty'  = []
  mappend' = (++)

-- newtype for use with monoid.
newtype Sum a = Sum a
  deriving (Eq, Ord, Num, Show)

getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Monoid' (Sum a) where
  mempty'  = Sum 0
  mappend' = (+)

-- newtype for use with monoid.
newtype Product a = Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a => Monoid' (Product a) where
  mempty'  = Product 1
  mappend' = (*)

lst :: [Integer]
lst = [1,5,8,23,423,99]

-- Gets the product of the list of Integers lst using mconcat'.
prod :: Integer
prod = getProduct . mconcat' . map Product $ lst

-- Pairs form a monoid as long as the individual components do:

instance (Monoid' a, Monoid' b) => Monoid' (a,b) where
  mempty'                = (mempty', mempty')
  (a,b) `mappend'` (c,d) = (a `mappend'` c, b `mappend'` d)

-- Challenge: Instance of Monoid' for Bool.

-- FIRST ATTEMPT

-- newtype And = And Bool
--   deriving (Eq, Show)
--
-- getAnd :: And -> Bool
-- getAnd (And a) = a
--
-- instance Monoid' And where
--   mempty'      = And True
--   mappend' a b = And $ (getAnd a) && (getAnd b)
--
-- newtype Or = Or Bool
--   deriving (Eq, Show)
--
-- getOr :: Or -> Bool
-- getOr (Or a) = a
--
-- instance Monoid' Or where
--   mempty'      = Or False
--   mappend' a b = Or $ (getOr a) || (getOr b)

-- SECOND ATTEMPT
-- (Adapted from: https://en.wikibooks.org/wiki/Haskell/Solutions/Monoids)

-- AND
newtype All = All Bool
  deriving (Eq, Show)

instance Monoid' All where
  mempty'                  = All True
  mappend' (All a) (All b) = All (a && b)

-- OR
newtype Any = Any Bool
  deriving (Eq, Show)

instance Monoid' Any where
  mempty'                  = Any False
  mappend' (Any a) (Any b) = Any (a || b)

-- XOR
newtype OnlyOne = OnlyOne Bool
  deriving (Eq, Show)

instance Monoid' OnlyOne where
  mempty'                  = OnlyOne False
  mappend' (OnlyOne a) (OnlyOne b) = OnlyOne (a /= b)

-- Challenge: Instance of Monoid' for function types.
-- (Need to more thoroughly understand Functor and Applicative?)

-- mempty'  = id  ?
-- mappend' = (.) ?

-- Above is to say: at empty, use a function that returns it's arguments;
-- and at append, use function composition to compose the two functions.
