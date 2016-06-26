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

{- Setup:
     Discs of different sizes are stacked on the first of three pegs.
     The objective is to move them all to the last peg.
   Rules:
     - May only move one disc at a time.
     - A larger disc may never be stacked on top of a smaller one.
   Solution for n discs from peg a to b using c as temporary storage:
     - Move n - 1 discs from a to c using b as temporary storage.
     - Move the top disc from a to b.
     - Move n - 1 discs from c to b using a as temporary storage. -}

{- These are type synonyms. They are more descriptive and help with
   documentation. -}
type Peg = String
type Move = (Peg, Peg)

-- Concatenates two lists of the same type.
concatTwoLists :: [a] -> [a] -> [a]
concatTwoLists [] ys     = ys
concatTwoLists (x:[]) ys = x : ys
concatTwoLists (x:xs) ys = x : concatTwoLists xs ys

{- Given the number of discs and names for the three pegs, returns
   a list of moves to be performed to move the stack of discs from
   the first peg to the second. -}
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = (a,b) : []
hanoi 2 a b c = (a,c) : (a,b) : hanoi 1 c b a
hanoi n a b c = concatTwoLists (concatTwoLists (hanoi (n - 1) a c b) (hanoi 1 a b c)) (hanoi (n - 1) c b a)



{----------------------------------------------------------
  Exercise 6 (Optional): Towers of Hanoi with four pegs.
----------------------------------------------------------}

{- Should solve in as few moves as possible.
   Minimal solution with 3 pegs: 32767 moves for 15 pegs.
   Minimal solution with 4 pegs: 129 moves for 15 pegs. -}



{- The below is dedicated to checking the validity of the hanoi
   result. -}



-- Disc represented by weight.
type Disc = Integer

-- Whether or not a peg is present in a list of (Peg, [Disc]) pairs.
pegPresent :: Peg -> [(Peg, [Disc])] -> Bool
pegPresent _ [] = False
pegPresent a ((p,_):xs)
  | a == p    = True
  | otherwise = pegPresent a xs

{- Whether or not a list of pegs is present in a list of
   (Peg, [Disc]) pairs. -}
pegsPresent :: [Peg] -> [(Peg, [Disc])] -> Bool
pegsPresent [] _      = error "Must check presence of at least one peg."
pegsPresent xs []     = False
pegsPresent (x:[]) ys = pegPresent x ys
pegsPresent (x:xs) ys
  | pegPresent x ys = pegsPresent xs ys
  | otherwise       = False

{- Whether or not at least one disc is present on a given peg in a
   list of (Peg, [Disc]) pairs. -}
discPresent :: Peg -> [(Peg, [Disc])] -> Bool
discPresent _ [] = False
discPresent a ((p,[]):xs)
  | a == p    = False
  | otherwise = discPresent a xs
discPresent a ((p,_):xs)
  | a == p    = True
  | otherwise = discPresent a xs

-- Gets the top disc from a (Peg, [Disc]) pair.
getDiscFromPeg :: (Peg, [Disc]) -> Disc
getDiscFromPeg (p, [])   = error "No discs."
getDiscFromPeg (p, d:ds) = d

-- Gets the top disc from a list of (Peg, [Disc]) pairs.
getDiscFromPegs :: Peg -> [(Peg, [Disc])] -> Disc
getDiscFromPegs a ((p,ds):xs)
  | not (pegPresent a ((p,ds):xs)) = error "Peg must be present."
  | p == a                         = getDiscFromPeg (p,ds)
  | otherwise                      = getDiscFromPegs a xs

-- Removes a disc from a (Peg, [Disc]) pair and yields the updated pair.
removeDiscFromPeg :: (Peg, [Disc]) -> (Peg, [Disc])
removeDiscFromPeg (p, [])   = error "No discs on the peg."
removeDiscFromPeg (p, d:ds) = (p, ds)

-- Removes a disc from a (Peg, [Disc]) pair and yields the updated pairs.
removeDiscFromPegs :: Peg -> [(Peg, [Disc])] -> [(Peg, [Disc])]
removeDiscFromPegs a ((p,ds):xs)
  | not (pegPresent a ((p,ds):xs)) = error "Peg must be present."
  | p == a                         = (removeDiscFromPeg (p,ds)) : xs
  | otherwise                      = (p,ds) : removeDiscFromPegs a xs

-- Adds a disc to a (Peg, [Disc]) pair and yields the updated pair.
addDiscToPeg :: Disc -> (Peg, [Disc]) -> (Peg, [Disc])
addDiscToPeg d (p, ds) = (p, (d:ds))

{- Adds a disc to a list of (Peg, [Disc]) pairs and yeilds the
   updated list of pairs. -}
addDiscByPeg :: Disc -> Peg -> [(Peg, [Disc])] -> [(Peg, [Disc])]
addDiscByPeg d a ((p,ds):xs)
  | not (pegPresent a ((p,ds):xs)) = error "Peg must be present."
  | p == a                         = addDiscToPeg d (p,ds) : xs
  | otherwise                      = (p,ds) : addDiscByPeg d a xs

-- Checks the legality of a hanoi move.
legalMove :: Move -> [(Peg, [Disc])] -> Bool
legalMove (a,b) []     = False
legalMove (a,b) (x:[]) = False
legalMove (a,b) xs
  | pegPresent a xs && pegPresent b xs = not (discPresent b xs) || (getDiscFromPegs a xs) < (getDiscFromPegs b xs)
  | otherwise                          = False

-- Makes a hanoi move and yields updated list of (Peg, [Disc]) pairs.
makeMove :: Move -> [(Peg, [Disc])] -> [(Peg, [Disc])]
makeMove (a,b) ps
  | legalMove (a,b) ps = addDiscByPeg (getDiscFromPegs a ps) b (removeDiscFromPegs a ps)
  | otherwise          = error "Cannot make an illegal move."

{- Make a list of hanoi moves and yields updated list of
   (Peg, [Disc]) pairs. -}
makeMoves :: [Move] -> [(Peg, [Disc])] -> [(Peg, [Disc])]
makeMoves [] xs = xs
makeMoves (m:ms) xs = makeMoves ms (makeMove m xs)

{-

0 =
1 =                                                            [(a,b)]
2 = [                        (a,c)                        ] -- [(a,b)] -- [                        (c,b)                        ]
3 = [                (a,b) : (a,c) : (b:c)                ] -- [(a,b)] -- [                (c,a) : (c,b) : (a,b)                ]
4 = [(a,c) : (a,b) : (c,b) : (a,c) : (b,a) : (b,c) : (a,c)] -- [(a,b)] -- [(c,b) : (c,a) : (b,a) : (c,b) : (a,c) : (a,b) : (c,b)]

-}
