{- Styleguide:
     - DO use camelCase for function and variable names.
     - DO use descriptive function names.
     - DON'T use tabs.
     - DO try to keep every line under 80 characters.
     - DO give every top-level function a type signature. (It doesn't
     hurt to give them to locally defined functions and constants as
     well.)
     - DO precede every top-level function by an explanatory comment.
     - DO use -Wall (no warnings).
     - DO use small functions and compose them to add complexity.
     - DO make all your functions total (sensible results for every
     input). -}

-- Validating Credit Card Numbers
{- Steps:
     - Double the value of every second digit beginning from the
     right.
       - Example: [1,3,8,6] becomes [2,3,16,6].
     - Add the digits of the result.
       - Example: [2,3,16,6] becomes 2+3+1+6+6 = 18.
     - Calculate the remainder when the result is divided by 10.
     - If the result is 0, the number is valid. -}



{----------------------------------------------------------
  Exercise 1: Find the digits of a number.
----------------------------------------------------------}

-- Gets the last element from a list.
getLast :: [a] -> a
getLast []     = error "List cannot be empty."
getLast (x:[]) = x
getLast (x:xs) = getLast xs

-- Removes the last element from a list.
removeLast :: [a] -> [a]
removeLast []     = []
removeLast (x:[]) = []
removeLast (x:xs) = x : removeLast xs

-- Reverses a list.
reverseList :: [a] -> [a]
reverseList [] = []
reverseList xs = getLast xs : reverseList (removeLast xs)

{- Converts positive Integers to a list of digits with the digits
   reversed. -}
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n < 1     = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

-- Converts positive Integers to a list of digits.
toDigits :: Integer -> [Integer]
toDigits n = reverseList (toDigitsRev n)



{----------------------------------------------------------
  Exercise 2: Double every other digit right to left.
----------------------------------------------------------}

-- Doubles every other number starting from the left.
doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:zs) = x : y * 2 : doubleEveryOtherRev zs

-- Doubles every other number starting from the right.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverseList (doubleEveryOtherRev (reverseList xs))



{----------------------------------------------------------
  Exercise 3: Calculate the sum of all digits.
----------------------------------------------------------}

-- Calculates the sum of all digits.
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs)
  | x < 10    = x + sumDigits xs
  | otherwise = sumDigits (toDigits x) + sumDigits xs



{----------------------------------------------------------
  Exercise 4: Determine whether a credit card number could be valid.
----------------------------------------------------------}

-- Determines whether a credit card number could be valid.
validate :: Integer -> Bool
validate n
  | n < 0     = False
  | otherwise = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0



{----------------------------------------------------------
  Exercise 5: The Towers of Hanoi.
----------------------------------------------------------}
