{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Exercise 1 -------------------------------------------------------

-- Applies a function to the first value of a pair.
first :: (a -> b) -> (a, c) -> (b, c)
first f (a, b) = (f a, b)

-- Functor instance for Parser.
instance Functor Parser where
  fmap f p = Parser (fmap (first f) . runParser p)

-- runParser (fmap (+ 1) posInt) "10ab1"
--   = Just (11, "ab1")

-- Exercise 2 -------------------------------------------------------

-- Applicative instance for Parser.
instance Applicative Parser where
  pure a    = Parser (\str -> Just (a, str))
  p1 <*> p2 = Parser (\str -> case runParser p1 str of
                                Nothing        -> Nothing
                                Just (f, rest) -> first f <$> runParser p2 rest)
                                  -- `runParser p2 rest` produces type Maybe.
                                  -- `<$>` then calls the Maybe instance of
                                  -- fmap, not the Parser instance.

-- runParser (pure 5) "abc"
--   = Just (5, "abc")
-- runParser ((fmap (+) posInt) <*> (pure 3)) "55"
--   = Just (58, "")

-- Exercise 3 -------------------------------------------------------

-- Expects the characters 'a' and 'b', and returns them as a pair.
-- (Note: Implement using the Applicative interface.)
abParser :: Parser (Char, Char)
abParser = combine <$> (char 'a') <*> (char 'b')
  where
    combine = (\a b -> (a, b))

-- Like abParser, except returns ().
abParser_ :: Parser ()
abParser_ = empty <$> abParser
  where
    empty = (\_ -> ())

-- Expects two integers separated by a space, and returns them in a list.
intPair :: Parser [Integer]
intPair = buildList <$> posInt <*> space <*> posInt
  where
    space    = char ' '
    buildList = (\a _ b -> [a, b])

-- Exercise 4 -------------------------------------------------------

{- class Applicative f => Alternative f where
     empty :: f a
     -- Represents choice.
     (<|>) :: f a -> f a -> f a -}

instance Alternative Parser where
  empty     = Parser (\_ -> Nothing)
  -- p1 <|> p2 = Parser (\str -> case runParser p1 str of
  --                               Nothing   -> runParser p2 str
  --                               something -> something)
  p1 <|> p2 = Parser (\str -> (runParser p1 str) <|> (runParser p2 str))
                              -- Uses Alternative instance for Maybe.

-- Exercise 5 -------------------------------------------------------

-- Parses either an integer or uppercase letter, fails otherwise.
intOrUppercase :: Parser ()
intOrUppercase = empty <$> (posIntStr <|> upperStr)
  where
    empty     = (\_ -> ())
    posIntStr = show <$> posInt
    upperStr  = show <$> (satisfy isUpper)
    -- <|> requires same types, using show here to convert to String as
    -- the same type.

-- runParser intOrUppercase "342abcd"
--   = Just((), "abcd")
-- runParser intOrUppercase "XYZ"
--   = Just((), "YZ")
-- runParser intOrUppercase "foo"
--   = Nothing
