import Data.List

{----------------------------------------------------------
  Anonymous Functions
----------------------------------------------------------}

-- Keeps only Integers from list that are > 100.
greaterThan100 :: [Integer] -> [Integer]
greaterThan100 xs = filter (\x -> x > 100) xs

-- Anonymous function (aka: lambda abstraction):
--   \x -> x > 100
-- Lambda abstraction with multiple arguments:
--   (\x y z -> [x, 2*y, 3*z]) 5 6 3

-- Using an operator section:
greaterThan100_3 :: [Integer] -> [Integer]
greaterThan100_3 xs = filter (>100) xs

-- Operator sections allow partial application of operators.
-- Examples:
--   (>100) 102
--   (100>) 102
--   map (*6) [1..5]

{----------------------------------------------------------
  Function Composition
----------------------------------------------------------}

-- Exercise:
--   Write a function with the type:
--     (b -> c) -> (a -> b) -> (a -> c)

foo :: (b -> c) -> (a -> b) -> (a -> c)
foo f g = \x -> f (g x)
-- (Rewritten below.)

-- Surprise! The above is (.), and represents function composition.

-- Consider:
myTest :: [Integer] -> Bool
myTest xs = even (length (greaterThan100 xs))

-- Rewritten:
myTest' :: [Integer] -> Bool
myTest' = even . length . greaterThan100

{----------------------------------------------------------
  Currying and Partial Application
----------------------------------------------------------}

-- Type of (.):
--   (.) :: (b -> c) -> (a -> b) -> a -> c
--   Note: No parentheses around a -> c

-- All functions take only one argument.
{- "Multi-argument functions" are actually outputting
   functions that take one argument themselves until
   the result is reached. -}
-- "Multi-argument" lambda abstraction:
--   \x y z -> ...
-- Is syntactic sugar for:
--   \x -> (\y -> (\z -> ...))
-- Similarly:
--   f x y z = ...
-- Is:
--   f = \x -> (\y -> (\z -> ...))

-- Rewrite of foo (.)
foo' :: (b -> c) -> (a -> b) -> a -> c
foo' f g x = f (g x)

{- Currying is the idea of representing multi-argument functions
   as one-argument functions returning functions. -}

-- To represent a function of two arguments:
f :: (Int, Int) -> Int
f (x,y) = 2*x + y

-- To convert between two representations of a two-argument function:
--   curry and uncurry
--     Defined like so:

finkel :: ((a,b) -> c) -> a -> b -> c
finkel f x y = f (x,y)

unfinkel :: (a -> b -> c) -> (a,b) -> c
unfinkel f (x,y) = f x y

-- Example: unfinkel (+) (2,3)
-- Example: finkel f 2 3

{- Regarding partial application:

   Partial application is very easy, but partially applying to an
   argument other than the first (except for infix operators) isn't
   very easy.

   Therefore, arguments should be ordered from least to greatest
   variation. -}

-- Example of the power of wholemeal style programming:

foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

-- Problems with above:
--   Too much at once.
--   Too low level.

-- Thinking about making incremental transformations of entire input:

foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> 7*x + 2) . filter (>3)

{----------------------------------------------------------
  Folds
----------------------------------------------------------}

-- Consider:

sum' :: [Integer] -> Integer
sum' []     = 0
sum' (x:xs) = x + sum' xs

product' :: [Integer] -> Integer
product' []     = 1
product' (x:xs) = x * product' xs

length' :: [a] -> Int
length' []     = 0
length' (_:xs) = 1 + length' xs

-- Abstracted:

fold :: b -> (a -> b -> b) -> [a] -> b
fold z f []     = z
fold z f (x:xs) = f x (fold z f xs)
-- Notice:
--   fold f z [a,b,c] == a `f` (b `f` (c `f` z))

sum''     = fold 0 (+)
product'' = fold 1 (*)
length''  = fold 0 (\_ s -> 1 + s)
length''' = fold 0 (\_ -> (1+))

-- Defined in prelude:
--   foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
--   foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c

-- Should use foldl' from Data.List instead generally (more efficient).
-- Note: The Haskell wiki advocates foldr in most cases.
--   https://wiki.haskell.org/Foldr_Foldl_Foldl'

-- From Haskell wiki, consider:

-- ~422 ms, ~400 mb
ex01 = foldl' (\a e -> (mod e 10)*a) 1 [1..10^7]
-- ~2203 ms, ~550 mb
ex02 = foldr (\e a -> (mod e 10)*a) 1 [1..10^7]
-- ~0 ms, ~50 kb
ex03 = foldr (\e a -> if mod e 10==0 then 0 else (mod e 10)*a) 1 [1..]
-- ~781 ms, ~500 mb
ex04 = foldl' (\a e -> if mod e 10==0 then 0 else (mod e 10)*a) 1 [1..10^7]
-- Faster than 04, but far slower than 03.
ex05 = foldl (\a e -> if mod e 10==0 then 0 else (mod e 10)*a) 1 [1..10^7]

{- ex01 evaluates each result first then continues, avoiding a
   build up of nested expressions.

   ex02 builds up a huge stack of nested expressions before
   evaluating the result.

   ex03 "short-circuits" to the answer immediately when possible.

   ex04 cannot "short-circuit" and must evaluate the entire input
   list.

   ex05 (I think) builds up a huge stack of nested expressions,
   like ex02 does, then evaluates them until it hits a result
   that allows it to "short-circuit" to the answer. -}
