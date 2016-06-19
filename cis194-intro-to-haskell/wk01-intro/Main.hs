-- Comments

-- This is a single-line comment.
{- This is a
   multi-line comment. -}


-- Declarations and Variables

-- x has type Int
x :: Int
x = 3
-- The below is an error. (x is immutable.)
-- x = 4
y :: Int
-- Makes REPL hang (infinite?).
y = y + 1


-- Basic Types

-- Machine-sized integers. (Size limited by architecture: x86, x64.)
i :: Int
i = -78
biggestInt, smallestInt :: Int
biggestInt = maxBound
smallestInt = minBound
-- Arbitrary-precision integers. (Size limited only by memory.)
n :: Integer
n = 1234567890987654321987340982334987349872349874534
reallyBig :: Integer
reallyBig = 2^(2^(2^(2^2)))
numDigits :: Int
numDigits = length (show reallyBig)
-- Double-precision floating point.
d1, d2 :: Double
d1 = 4.58387
d2 = 6.2831e-4
-- Single-precision floating point.
f :: Float
f = 5.25
-- Booleans.
b1, b2 :: Bool
b1 = True
b2 = False
-- Unicode characters.
c1, c2, c3 :: Char
c1 = 'x'
c2 = 'Ø'
c3 = 'ダ'
-- Strings. (Lists of characters with special syntax.)
s :: String
s = "Hello, Haskell!"


-- Arithmetic

ex01 = 3 + 2
ex02 = 19 - 27
ex03 = 2.35 * 8.6
ex04 = 8.7 / 3.1
ex05 = mod 19 3
-- Backticks make a function name into an infix operator.
ex06 = 19 `mod` 3
ex07 = 7 ^ 222
ex08 = (-3) * (-7)
{- Haskell doesn't do implicit conversion.
   (Use fromIntegral, round, floor, and ceiling.) -}
-- bad = i + n
{- The / operator is for floating point numbers,
   Use div for integers. -}
-- bad2 = i / i
ex09 = i `div` i
ex10 = 12 `div` 5


-- Boolean Logic

ex11 = True && False
ex12 = not (False || True)
ex13 = ('a' == 'a')
ex14 = (16 /= 3)
ex15 = (5 > 3) && ('p' <= 'q')
ex16 = "Haskell" > "C++"
{- There are also if-expressions:
     if b then t else f
   These are rare though, with pattern-matching and
   guards often used instead. -}


-- Basic Functions

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)
{- sumtorial :: Integer -> Integer says:
     function sumtorial takes an Integer and yields another
     Integer. -}
{- sumtorial 0 = 0 is a clause:
     Clauses are evaluated top to bottom with the first
     matching clause chosen. A variable like n matches
     anything. -}
