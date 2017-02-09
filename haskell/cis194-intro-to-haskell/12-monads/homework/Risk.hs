{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int }
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }

-- Exercise 1 -------------------------------------------------------

-- Install and inspect MonadRandom.

-- Exercise 2 -------------------------------------------------------

instance Show Battlefield where
  show bf = "Attackers: " ++ show (attackers bf) ++ " Defenders: " ++ show (defenders bf)

instance Num Battlefield where
  (+) a b     = Battlefield (attackers a + attackers b) (defenders a + defenders b)
  (-) a b     = Battlefield (attackers a - attackers b) (defenders a - defenders b)

instance Eq Battlefield where
  (==) a b = ((attackers a) == (attackers b)) && ((defenders a) == (defenders b))

-- Gets multiple dice rolls.
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

-- Gets a battlefield comprised only of units capable of engaging in a single battle.
getBattle :: Battlefield -> Battlefield
getBattle bf = Battlefield atk def
  where
    atk = (min (attackers bf - 1) 3)
    def = (min (defenders bf) 2)

-- Resolves a single battle.
resolve :: Battlefield -> [DieValue] -> Battlefield
resolve bf xs = bf - casualties
  where
    casualties = Battlefield failedAttacks (min successfulAttacks (defenders bf))
    failedAttacks = length . filter (uncurry (<=)) $ zip atks defs
    successfulAttacks = length . filter (uncurry (>)) $ zip atks defs
    atks = reverse . sort $ take (attackers bf) xs
    defs = reverse . sort . take (defenders bf) $ drop (length atks) xs

-- Simulates a single battle. Assumes maximum number of units are used.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = dice 5 >>= (\xs -> return (resolve battlers xs)) >>= newField
  where
    battlers = getBattle bf
    newField = (\resolved -> return (bf - battlers + resolved))

-- Simplest.
test01 = (resolve (Battlefield 1 1) ([DV 6] ++ [DV 1])) == (Battlefield 1 0)
-- Tied rolls.
test02 = (resolve (Battlefield 1 1) ([DV 6] ++ [DV 6])) == (Battlefield 0 1)
-- More attackers.
test03 = (resolve (Battlefield 2 1) ([DV 6, DV 6] ++ [DV 1])) == (Battlefield 2 0)
-- More defenders.
test04 = (resolve (Battlefield 1 2) ([DV 6] ++ [DV 1, DV 1])) == (Battlefield 1 1)
-- Realistic.
test05 = (resolve (Battlefield 3 2) ([DV 2, DV 5, DV 1] ++ [DV 3, DV 4])) == (Battlefield 2 1)

tests = [test01, test02, test03, test04, test05]
