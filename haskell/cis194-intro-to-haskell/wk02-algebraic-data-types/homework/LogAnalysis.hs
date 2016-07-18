{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- Exercise 1 -------------------------------------------------------

-- Parses a single log message. (Note: Fails if "read t" or "read l" doesn't produce an Int.)
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
-- insert :: LogMessage -> MessageTree -> MessageTree
