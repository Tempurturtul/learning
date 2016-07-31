{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1 -------------------------------------------------------

-- Parses a single log message.
-- Note: Fails if "read t" or "read l" doesn't produce an Int.
parseMessage :: String -> LogMessage
parseMessage s = case (words s) of
                   ("I":t:m)   -> LogMessage Info (read t) (unwords m)
                   ("W":t:m)   -> LogMessage Warning (read t) (unwords m)
                   ("E":l:t:m) -> LogMessage (Error (read l)) (read t) (unwords m)
                   _           -> Unknown s

-- Parses an entire log file.
parse :: String -> [LogMessage]
parse s = [parseMessage l | l <- lines s]

-- Exercise 2 -------------------------------------------------------

-- Inserts a new LogMessage into an existing MessageTree.
-- Note: MessageTree input and output must be sorted by TimeStamp.
-- Note: An Unknown LogMessage will return the tree unchanged.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree = tree
insert (LogMessage a b c) Leaf = Node Leaf (LogMessage a b c) Leaf
insert (LogMessage a b c) (Node ltree (LogMessage x y z) rtree)
  | y > b     = (Node (insert (LogMessage a b c) ltree) (LogMessage x y z) rtree)
  | otherwise = (Node ltree (LogMessage x y z) (insert (LogMessage a b c) rtree))
insert _ (Node _ (Unknown _) _) = error "MessageTree contains an Unknown LogMessage."

-- Exercise 3 -------------------------------------------------------

-- Builds a MessageTree.
build :: [LogMessage] -> MessageTree
build []     = Leaf
build (x:xs) = insert x (build xs)

-- Exercise 4 -------------------------------------------------------

-- Perform an in-order traversal of the MessageTree.
-- Note: There are better ways to sort a list, this is an exercise in recursive data structures.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node ltree (LogMessage a b c) rtree) = inOrder ltree ++ [LogMessage a b c] ++ inOrder rtree
inOrder (Node _ (Unknown _) _) = error "MessageTree contains an Unknown LogMessage."

-- Exercise 5 -------------------------------------------------------

-- Gets timestamp sorted messages for errors of >= 50 severity.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong [] = []
whatWentWrong xs = [b | (LogMessage (Error a) _ b) <- (inOrder (build xs)), a >= 50]

-- Exercise 6 (Optional) --------------------------------------------

-- TODO
