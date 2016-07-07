{----------------------------------------------------------
  Enumeration Types
----------------------------------------------------------}

-- Declare Thing type with five data constructors.
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show  -- Tells GHC to convert Things to Strings.

shoe :: Thing
shoe = Shoe

listO'Things :: [Thing]
listO'Things = [Shoe, SealingWax, King, Cabbage, King]

isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True

{----------------------------------------------------------
  Beyond Enumerations
----------------------------------------------------------}

-- Declare a more general algebraic data type (not just an enumeration).
data FailableDouble = Failure
                    | OK Double
  deriving Show
-- FailableDouble takes two data constructors, Failure and OK.
-- OK takes a Double argument.
-- OK 3.4 is a value of type FailableDouble, as is Failure.

ex01 = Failure
ex02 = OK 3.4

safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

failureToZero :: FailableDouble -> Double
failureToZero Failure = 0
failureToZero (OK d)  = d

-- Data constructors can have more than one argument.

-- Store a person's name, age, and favorite Thing.
data Person = Person String Int Thing
  deriving Show

-- Note on the above:
--   The type constructor and data constructor are both named Person.
--   They inhabit different namespaces and are different things.
--   This is a common idiom.

brent :: Person
brent = Person "Brent" 31 SealingWax

stan :: Person
stan = Person "Stan" 94 Cabbage

getAge :: Person -> Int
getAge (Person _ a _) = a

{----------------------------------------------------------
  Algebraic Data Types in General
----------------------------------------------------------}

-- Can have one of more data constructors, each with zero or more arguments.
-- Type and data construcor names must always start with a capital letter.

-- data AlgDataType = Constr1 Type11 Type12
--                  | Constr2 Type21
--                  | Constr3 Type31 Type32 Type33
--                  | Constr4

{----------------------------------------------------------
  Pattern-Matching
----------------------------------------------------------}

{- Pattern-matching is about taking apart a value by finding
   out which constructor it was built with. -}

-- foo (Constr1 a b)   = ...
-- foo (Constr2 a)     = ...
-- foo (Constr3 a b c) = ...
-- foo Constr4         = ...

{- x@pat can be used to match a value against the pattern pat, while
   also giving the name x to the entire value being matched. -}

baz :: Person -> String
baz p@(Person n _ _) = "The name field of (" ++ show p ++ ") is " ++ n

-- Patterns can be nested.

checkFav :: Person -> String
checkFav (Person n _ SealingWax) = n ++ ", you're my kind of person!"
checkFav (Person n _ _) = n ++ ", your favorite thing is lame."

{----------------------------------------------------------
  Case Expressions
----------------------------------------------------------}

-- Fundamental construct for doing pattern-matching.

-- case exp of
--   pat1 -> exp1
--   pat2 -> exp2
--   ...

ex03 = case "Hello" of
         []      -> 3
         ('H':s) -> length s
         _       -> 7

-- Syntax for defining functions used previously is sugar for case expressions.

failureToZero' :: FailableDouble -> Double
failureToZero' x = case x of
                     Failure -> 0
                     OK d    -> d

{----------------------------------------------------------
  Recursive Data Types
----------------------------------------------------------}

data IntList = Empty | Cons Int IntList

intListProd :: IntList -> Int
intListProd Empty      = 1
intListProd (Cons x l) = x * intListProd l

-- Binary tree with Int at each internal node and Char at each leaf.
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))
