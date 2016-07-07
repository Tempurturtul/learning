-- Exercise 1 -------------------------------------------------------

-- Converts positive Integers to a list of digits.
toDigits :: Integer -> [Integer]
toDigits n
  | n < 1     = []
  | otherwise = toDigits (div n 10) ++ [mod n 10]

-- Converts positive Integers to a list of digits reversed.
toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

-- Exercise 2 -------------------------------------------------------

-- Doubles every other digit left to right.
doubleEveryOtherLTR :: [Integer] -> [Integer]
doubleEveryOtherLTR []       = []
doubleEveryOtherLTR (x:[])   = [x]
doubleEveryOtherLTR (x:y:zs) = x : y*2 : doubleEveryOtherLTR zs

-- Doubles every other digit right to left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs = reverse (doubleEveryOtherLTR (reverse xs))

-- Exercise 3 -------------------------------------------------------

-- Sums digits.
sumDigits :: [Integer] -> Integer
sumDigits []  = 0
sumDigits (x:xs)
  | x > 9     = sumDigits (toDigits x) + sumDigits xs
  | otherwise = x + sumDigits xs

-- Exercise 4 -------------------------------------------------------

-- Checks if Integer could be a valid credit card number.
validate :: Integer -> Bool
validate n
  | n < 0     = False
  | otherwise = mod (sumDigits (doubleEveryOther (toDigits n))) 10 == 0

-- Exercise 5 -------------------------------------------------------

-- These are type synonyms.
-- They are more descriptive and help with documentation.
type Peg = String
type Move = (Peg, Peg)

-- Gets the solution to a Towers of Hanoi puzzle.
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a,b)] ++ (hanoi (n-1) c b a)

-- Exercise 6 -------------------------------------------------------



-- Additional: Checking 5 & 6 ---------------------------------------

-- Usage:
--   solves (hanoi n a b c) (assembleBoard n [a, b, c])
--   putStr (drawMoves (hanoi n a b c) (assembleBoard n [a, b, c]))

type Disc = Integer
type Board = [(Peg, [Disc])]

-- Assembles a new Towers of Hanoi Board.
assembleBoard :: Integer -> [Peg] -> Board
assembleBoard _ []     = []
assembleBoard n (x:xs)
  | n < 1     = (x, []) : assembleBoard 0 xs
  | otherwise = (x, [1..n]) : assembleBoard 0 xs

-- -- Gets all discs from the Hanoi board.
getDiscs :: Board -> [Disc]
getDiscs b = concat [xs | (_,xs) <- b]

-- Checks if Integers only increase.
onlyIncreases :: [Integer] -> Bool
onlyIncreases []     = True
onlyIncreases (_:[]) = True
onlyIncreases (x:xs) = x < head xs && onlyIncreases xs

-- Checks if a Towers of Hanoi board is solved. (Discs moved from first to second peg.)
solved :: Board -> Bool
solved []            = error "Cannot solve an empty board."
solved (_:[])        = error "Cannot solve a board with only one peg."
solved (x:(_,ds):zs) = getDiscs (x:zs) == [] && onlyIncreases ds

-- Checks for the peg on the board.
pegExists :: Peg -> Board -> Bool
pegExists _ [] = False
pegExists a ((p,_):xs)
  | a == p    = True
  | otherwise = pegExists a xs

-- Checks for pegs on the board.
pegsExist :: [Peg] -> Board -> Bool
pegsExist [] _     = True
pegsExist (x:xs) b = pegExists x b && pegsExist xs b

-- Checks for at least one disc on the peg.
discOnPeg :: Peg -> Board -> Bool
discOnPeg _ [] = False
discOnPeg a ((p,ds):xs)
  | a == p    = ds /= []
  | otherwise = discOnPeg a xs

-- The top disc from the peg.
topDisc :: Peg -> Board -> Disc
topDisc _ [] = error "Peg must exist in board."
topDisc a ((p,[]):xs)
  | a == p    = error "No disc on peg."
  | otherwise = topDisc a xs
topDisc a ((p,d:_):xs)
  | a == p    = d
  | otherwise = topDisc a xs

-- Checks if a hanoi move is legal.
legalMove :: Move -> Board -> Bool
legalMove (x,y) b
  | not (pegsExist [x,y] b) = False  -- Can't move to a missing peg.
  | not (discOnPeg x b)     = False  -- Can't move a missing disc.
  | not (discOnPeg y b)     = True   -- Can always move a disc to an empty peg.
  | otherwise               = (topDisc x b) < (topDisc y b)

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

-- Makes a move on a Towers of Hanoi board.
makeMove :: Move -> Board -> Board
makeMove (x,y) b
  | legalMove (x,y) b = addDisc (topDisc x b) y (removeDisc x b)
  | otherwise         = error "Illegal move."

-- Checks if moves solve a Towers of Hanoi puzzle.
solves :: [Move] -> Board -> Bool
solves [] b      = solved b
solves (x:xs) b  = solves xs (makeMove x b)

-- Additional: Visualizing 5 & 6 ------------------------------------

-- Gets the biggest number of discs on a peg.
mostDiscs :: Board -> Int
mostDiscs [] = 0
mostDiscs ((_,ds):xs)
  | length ds > mostDiscs xs = length ds
  | otherwise                = mostDiscs xs

-- Removes the heighest disc(s) from a board.
removeHeighestDiscs :: Board -> Board
removeHeighestDiscs [] = []
removeHeighestDiscs ((p,[]):xs) = (p,[]) : removeHeighestDiscs xs
removeHeighestDiscs ((p,d:ds):xs)
  | length (d:ds) > mostDiscs xs  = removeDisc p ((p,d:ds):xs)
  | length (d:ds) == mostDiscs xs = (p,ds) : removeHeighestDiscs xs
  | otherwise                     = (p,d:ds) : removeHeighestDiscs xs

-- Draws discs at given height as a string (0 is base, no disc is "|").
drawDiscsAtHeight :: Int -> Board -> String
drawDiscsAtHeight _ []          = "\n"  -- Nothing to draw.
drawDiscsAtHeight n _
  | n < 0                       = error "Disc height cannot be negative."
drawDiscsAtHeight n ((_,[]):xs) = "|" ++ drawDiscsAtHeight n xs  -- No discs on peg.
drawDiscsAtHeight n ((p,d:ds):xs)
  | length (d:ds) - 1 == n = show d ++ drawDiscsAtHeight n xs  -- Draw this disc.
  | otherwise              = drawDiscsAtHeight n ((p,ds):xs)   -- Wrong disc.

-- Draws a Towers of Hanoi board as a string.
drawBoard :: Board -> String
drawBoard [] = "\n"
drawBoard b
  | mostDiscs b == 0 = "\n"  -- Done drawing.
  | otherwise        = (drawDiscsAtHeight ((mostDiscs b) - 1) b) ++ (drawBoard (removeHeighestDiscs b))

-- Draws a Towers of Hanoi board as a string after each move.
drawMoves :: [Move] -> Board -> String
drawMoves [] _     = "\n"  -- Done drawing.
drawMoves (x:xs) b = drawBoard (makeMove x b) ++ (drawMoves xs (makeMove x b))
