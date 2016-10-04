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
