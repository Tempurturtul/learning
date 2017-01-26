{----------------------------------------------------------
  Motivation
----------------------------------------------------------}

-- What if we want to decide what to do based on some intermediate results?

-- Applicative gives no way to decide what to do next based on previous
-- results. For example, we can't use Applicative to read an input, then use
-- that input to determine how many following inputs to read, then repeat. With
-- Applicative we have to decide in advance what operations we are going to run
-- before we can see the results.

{----------------------------------------------------------
  Monad
----------------------------------------------------------}

-- class Monad m where
--   return :: a -> m a
--
--   (>>=) :: m a -> (a -> m b) -> m b
--
--   (>>) :: m a -> m b -> m b
--   m1 >> m2 = m1 >>= \_ -> m2

-- The above should look familiar (week 8: IO).

-- return has the same type as Applicative's pure

-- There is a fourth method called fail, but it was a mistake and should never
-- be used. (According to Yorgey.)

-- (>>=)
--   Pronounced: bind
--   Takes two arguments.
--     m a
--       Called a monadic value, computation, or mobit. (Not monad!)
--       Represents a computation that results in a value of type a, and may
--       also have some sore of "effect".
--     (a -> m b)
--       Function that will choose the next computation to run based on the
--       results of the first computation.
--   Essentially puts together two mobits to produce a larger one.

-- (>>)
--   Simply performs the first mobit, ignores the result, then performs the
--   second mobit.

{----------------------------------------------------------
  Examples
----------------------------------------------------------}

-- instance Monad Maybe where
--   return        = Just
--   Nothing >>= _ = Nothing
--   Just x >>= k  = k x

-- Examples:

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01 = return 7 >>= check >>= halve
ex02 = return 12 >>= check >>= halve
ex03 = return 12 >>= halve >>= check

-- instance Monad [] where
--   return x = [x]
--   xs >>= k = concat (map k xs)

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x+1, x+2]

ex04 = [10,20,30] >>= addOneOrTwo

{----------------------------------------------------------
  Monad Combinators
----------------------------------------------------------}

-- sequence takes a list of monadic values and produces a single monadic value
-- which collects the results.

-- sequence :: Monad m => [m a] -> m [a]
-- sequence [] = return []
-- sequence (ma:mas) =
--   ma >>= \a ->
--   sequence mas >>= \as ->
--   return (a:as)

-- sequence can be used to write other combinators.

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n m = sequence (replicate n m)

-- We now have the tools to write a parser that can do things based on previous
-- results!

-- parseFile :: Parser [[Int]]
-- parseFile = many parseLine
--
-- parseLine :: Parser [Int]
-- parseLine = parseInt >>= \i -> replicateM i parseInt

-- (many was zeroOrMore in week 11's homework)
