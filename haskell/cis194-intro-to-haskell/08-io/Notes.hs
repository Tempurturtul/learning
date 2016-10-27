{----------------------------------------------------------
  The Problem with Purity
----------------------------------------------------------}

{- Because Haskell is lazy and pure:
     1. Functions may not have external effects.
       - Example: Printing to screen.
     2. Functions may not depend on external things.
       - Example: Input from keyboard. -}

{----------------------------------------------------------
  The IO Type
----------------------------------------------------------}

{-  Consider:
      -- A cake.
      c :: Cake
      -- A recipe for a cake (not an actual cake).
      r :: Recipe Cake -}

-- Similarly, a value of type IO a is just a "recipe" for
-- producing a value of type a.

-- The special value "main :: IO ()" is the only way a value
-- of type IO is actually executed.

-- Given a string, returns an IO computation that will (when
-- executed) print out that String on the screen.
putStrLn :: String -> IO ()

{----------------------------------------------------------
  There is no String "Inside" an IO String
----------------------------------------------------------}

-- A value of type IO String is a "recipe" for a String, it does
-- not "contain" a String.

{----------------------------------------------------------
  Combining IO
----------------------------------------------------------}

-- "And then" operator.
(>>) :: IO a -> IO b -> IO b

-- Regarding "and then" operator:
--   - Creates an IO computation that consists of running the two
--   input computations in sequence.
--   - Results of first computation are discarded.
--     - (Only care about effects.)
--   - Example:
--     - main = putStrLn "Hello" >> putStrLn "world!"

-- "Bind" operator.
(>>=) :: IO a -> (a -> IO b) -> IO b

-- Regarding "bind" operator:
--   - Takes a computation that will produce a value of type a,
--   and a function which gets to compute a second computation
--   based on the intermediate value of type a.
--   - Example:
--     - main :: IO ()
--     - main = putStrLn "Please enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))

{----------------------------------------------------------
  Record Syntax
----------------------------------------------------------}

-- Consider:

data D = C T1 T2 T3

-- Above declared with record syntax:

data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }

-- Record syntax specifies a name in addition to type for each
-- field stored inside the constructor.

-- Can be used in all the same ways.

{-  Additional benefits:
      1. Each field name is automatically a "projection function"
      which gets the value of that field out of a value of type D.
        - Example: field2 :: D -> T2
      2. Special syntax for constructing, modifying, and
      pattern-matching on values of type D (in addition to usual
      syntax).
        - Constructing:
          - C { field3 = ..., field1 = ..., field2 = ... }
            - ... filled in by expressions of right type, and fields
            defined in any order.
        - Modifying some d :: D:
          - d { field3 = ... }
            - Doesn't actually mutate d; rather constructs a new
            value of type D which is the same as d except with field3
            replaced by the given value.
        - Pattern-Matching:
          - foo (C { field1 = x }) = ... x ...
            - Matches only on the field1 field and ignores others. -}
