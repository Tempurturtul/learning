{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Scrabble where

import Data.Char

-- Exercise 3 (Also in JoinList) ------------------------------------

newtype Score = Score Int
  deriving (Eq, Ord, Num, Show)

instance Monoid Score where
  mempty  = Score 0
  mappend = (+)

-- Implements scrabble scoring values.
score :: Char -> Score
score c
  | length match > 0 = Score $ head match
  | otherwise        = Score 0
  where one   = ("AEILNORSTU", 1)
        two   = ("DG", 2)
        three = ("BCMP", 3)
        four  = ("FHVWY", 4)
        five  = ("K", 5)
        eight = ("JX", 8)
        ten   = ("QZ", 10)
        lst   = [one, two, three, four, five, eight, ten]
        match = [val | (str, val) <- lst, elem (toUpper c) str]

-- Implements scrabble scoring values.
scoreString :: String -> Score
scoreString = mconcat . map score
