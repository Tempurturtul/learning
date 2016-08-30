{----------------------------------------------------------
  Strict Evaluation
----------------------------------------------------------}

-- Opposite of lazy evaluation.

{- Function arguments are completely evaluated before being
   passed along. -}

{- Example:

  f x y = x + 2

  In a strict language, x and y are completely evaluated, then
  the results are passed to f.

  In the above example y is unused by f, so the evaluation of
  y was unnecessary. -}

{----------------------------------------------------------
  Side Effects and Purity
----------------------------------------------------------}

{- The presence or absence of side effects determines the
   which method of evaluation is most appropriate. -}

{- Side effects (or outside interactions) are time-sensitive,
   meaning that strict evaluation is necessary to ensure that
   they occur when expected. -}

{- Many essential operations like taking input, printing
   output, and reading files are considered to be side
   effects. In order to remain pure while still allowing
   such essential side effects, Haskell uses things like the
   IO monad (discussed later). -}

{----------------------------------------------------------
  Lazy Evaluation
----------------------------------------------------------}

-- Evaluation of function arguments is delayed as long as possible.

-- Unevaluated expressions are package up and called "thunks".

{- Example:

  f x y = x + 2

  x and y are packaged up into thunks and f is called immediately,
  y is never used by f and is thrown away by the garbage collector.
-}

{----------------------------------------------------------
  Pattern Matching Drives Evaluation
----------------------------------------------------------}

{- Example:

   f1 :: Maybe a -> [Maybe a]
   f1 m = [m,m]

   f2 :: Maybe a -> [a]
   f2 Nothing  = []
   f2 (Just x) = [x]

   f1 doesn't need to know anything about its argument. m can remain
   unevaluated. This is to say that f1 e does not depend on the
   "shape" of e.

   f2 needs to know something about its argument. m must be evaluated
   because the result of f2 depends on the "shape" of e.

   Note that thunks are evaluated only enough to allow a pattern
   match to proceed, no farther. -}

{----------------------------------------------------------
  Consequences
----------------------------------------------------------}

-- Forced to choose purity.

-- Tricky to reason about space usage.
{- Example:

     foldl (+) 0 [1,2,3]
   = foldl (+) (0+1) [2,3]
   = foldl (+) ((0+1)+2) [3]
   = foldl (+) (((0+1)+2)+3) []
   = (((0+1)+2)+3)
   = ((1+2)+3)
   = (3+3)
   = 6

   vs

     fold' (+) 0 [1,2,3]
   = fold' (+) (0+1) [2,3]
   = fold' (+) 1 [2,3]
   = fold' (+) (1+2) [3]
   = fold' (+) 3 [3]
   = fold' (+) (3+3) []
   = fold' (+) 6 []
   = 6

   The stack in the first example builds up all the unevaluated
   expressions before beginning to evaluate them, which can cause a
   stack overflow in longer lists.
-}

-- Short-circuiting operators (like && and ||) aren't special cases.

-- Infinite data structures.

-- "Pipelining" incremental transformations can be memory efficient.

-- Easier dynamic programming. (Problem -> Subproblems -> Solutions -> Solution)
-- Example:

import Data.Array

knapsack01 :: [Double]  -- values
           -> [Integer] -- nonnegative weights
           -> Integer   -- knapsack size
           -> Double    -- max possible value
knapsack01 vs ws maxW = m!(numItems-1, maxW)
  where numItems = length vs
        m = array ((-1,0), (numItems-1, maxW)) $
              [((-1,w), 0) | w <- [0 .. maxW]] ++
              [((i,0), 0) | i <- [0 .. numItems-1]] ++
              [((i,w), best)
                  | i <- [0 .. numItems-1]
                  , w <- [1 .. maxW]
                  , let best
                          | ws!!i > w = m!(i-1, w)
                          | otherwise = max (m!(i-1, w))
                                            (m!(i-1, w - ws!!i) + vs!!i)
              ]

example = knapsack01 [3,4,5,8,10] [2,3,4,5,9] 20
-- 26.0
