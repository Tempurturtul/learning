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
hanoi n a b c = (hanoi (n - 1) a c b) `concatTwoLists` (hanoi 1 a b c) `concatTwoLists` (hanoi (n - 1) c b a)

{- Note on the above: I stumbled upon this answer while going through
   successive cases looking for a pattern. I didn't expect it to
   work to be honest. Playing with stackable measuring cups helped me
   understand why it works. -}



{----------------------------------------------------------
  Exercise 6 (Optional): Towers of Hanoi with four pegs.
----------------------------------------------------------}

{- Should solve in as few moves as possible.
   Minimal solution with 3 pegs: 32767 moves for 15 pegs.
   Minimal solution with 4 pegs: 129 moves for 15 pegs. -}



{----------------------------------------------------------
  The below is dedicated to checking the validity of the
  hanoi results.
  (Usage: makeMoves (hanoi n a b c) (assembleBoard n [a, b, c]))
----------------------------------------------------------}

-- Disc represented by weight.
type Disc = Integer
type Tower = (Peg, [Disc])
type Board = [Tower]

-- Whether or not a peg exists in a board.
pegExists :: Peg -> Board -> Bool
pegExists _ [] = False
pegExists a ((p,_):xs)
  | a == p    = True
  | otherwise = pegExists a xs

-- Whether or not pegs exist in a board.
pegsExist :: [Peg] -> Board -> Bool
pegsExist [] _     = True
pegsExist (x:xs) b = pegExists x b && pegsExist xs b

-- Whether or not there is at least one disc on the peg.
discOnPeg :: Peg -> Board -> Bool
discOnPeg a [] = False
discOnPeg a ((p,ds):xs)
  | a == p    = ds /= []
  | otherwise = discOnPeg a xs

-- The top disc from a peg.
topDisc :: Peg -> Board -> Disc
topDisc _ [] = error "Peg must exist in board."
topDisc a ((p,[]):xs)
  | a == p    = error "No disc on peg."
  | otherwise = topDisc a xs
topDisc a ((p,d:ds):xs)
  | a == p    = d
  | otherwise = topDisc a xs

-- Adds a disc to a peg and yields an updated board.
addDisc :: Disc -> Peg -> Board -> Board
addDisc _ _ [] = error "Peg must exist in board."
addDisc d a ((p,ds):xs)
  | a == p    = ((p,d:ds):xs)
  | otherwise = (p,ds) : addDisc d a xs

-- Removes the top disc from a peg and yields an updated board.
removeDisc :: Peg -> Board -> Board
removeDisc _ [] = error "Peg must exist in board."
removeDisc a ((p,[]):xs)
  | a == p    = error "No disc on peg."
  | otherwise = (p,[]) : removeDisc a xs
removeDisc a ((p,d:ds):xs)
  | a == p    = ((p,ds):xs)
  | otherwise = (p,d:ds) : removeDisc a xs

-- Checks the legality of a move.
legalMove :: Move -> Board -> Bool
legalMove (x,y) b
  | not (pegsExist [x,y] b) = False
  | not (discOnPeg x b)     = False
  | not (discOnPeg y b)     = True
  | otherwise               = (topDisc x b) < (topDisc y b)

-- Makes a move.
makeMove :: Move -> Board -> Board
makeMove (x,y) b
  | legalMove (x,y) b = addDisc (topDisc x b) y (removeDisc x b)
  | otherwise         = error "Illegal move."

-- Makes moves.
makeMoves :: [Move] -> Board -> Board
makeMoves [] b     = b
makeMoves (m:ms) b = makeMoves ms (makeMove m b)

-- Assembles a board with discs in starting position.
assembleBoard :: Integer -> [Peg] -> Board
assembleBoard _ []     = []
assembleBoard n (p:ps)
  | n > 0     = (p,[1..n]) : assembleBoard 0 ps
  | otherwise = (p,[]) : assembleBoard 0 ps

-- Gets the biggest number of discs on a peg.
mostDiscs :: Board -> Int
mostDiscs [] = 0
mostDiscs ((_,ds):xs)
  | length ds > mostDiscs xs = length ds
  | otherwise                = mostDiscs xs

{- Removes the top-most disc (or discs if there are more than one at
   the same height) from a board. -}
-- removeTop :: Board -> Board

-- Draws discs at given height (0 is base). No disc is "|".
-- drawDiscsAtHeight :: Int -> Board -> String
-- drawDiscsAtHeight _ []          = "\n"
-- drawDiscsAtHeight n _
--   | n < 0                       = error "Disc height cannot be negative."
-- drawDiscsAtHeight n ((_,[]):xs) = "|" ++ drawDiscsAtHeight n xs
-- drawDiscsAtHeight n ((p,d:ds):xs)
--   | length (d:ds) - 1 < n  = "|" ++ drawDiscsAtHeight n xs
--   | length (d:ds) - 1 == n = show d ++ drawDiscsAtHeight n xs
--   | otherwise              = drawDiscsAtHeight n ((p,ds):xs)

-- Draws a board.
-- drawBoard :: Board -> String
-- drawBoard [] = "\n"
-- drawBoard b  = drawDiscsAtHeight (mostDiscs b) b ++ drawBoard (removeTop b)

{-

0 =
1 =                                                            [(a,b)]
2 = [                        (a,c)                        ] -- [(a,b)] -- [                        (c,b)                        ]
3 = [                (a,b) : (a,c) : (b:c)                ] -- [(a,b)] -- [                (c,a) : (c,b) : (a,b)                ]
4 = [(a,c) : (a,b) : (c,b) : (a,c) : (b,a) : (b,c) : (a,c)] -- [(a,b)] -- [(c,b) : (c,a) : (b,a) : (c,b) : (a,c) : (a,b) : (c,b)]

-}
