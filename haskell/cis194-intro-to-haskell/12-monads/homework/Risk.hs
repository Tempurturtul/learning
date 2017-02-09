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

-- Gets multiple dice rolls.
dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)

-- Gets units capable of engaging in a battle.
getBattlers :: Battlefield -> (Army, Army)
getBattlers bf = (min (attackers bf - 1) 3, min (defenders bf) 2)

-- Gets multiple dice rolls.
getRolls :: Int -> Rand StdGen [DieValue]
getRolls n = sequence (replicate n die)

-- Resolves a battle.
resolve :: (Army, Army) -> [DieValue] -> (Army, Army)
resolve (atk, def) rolls = (atk - failed, def - successful)
  where
    failed      = length . filter (uncurry (<=)) $ pairedRolls
    successful  = length . filter (uncurry (>)) $ pairedRolls
    pairedRolls = zip atkRolls defRolls
    atkRolls    = reverse . sort $ take atk rolls
    defRolls    = reverse . sort $ take def (drop atk rolls)

-- Updates a battlefield to reflect the results of a battle.
update :: Battlefield -> (Army, Army) -> Battlefield
update bf result = Battlefield updatedAttackers updatedDefenders
  where
    updatedAttackers = (attackers bf) - (fst battlers) + (fst result)
    updatedDefenders = (defenders bf) - (snd battlers) + (snd result)
    battlers         = getBattlers bf

-- Simulates a single battle. Assumes maximum number of units are used.
battle :: Battlefield -> Rand StdGen Battlefield
battle bf = rolls >>= (\rs -> return (resolve battlers rs)) >>= (\armies -> return (update bf armies))
  where
    battlers = getBattlers bf
    rolls = dice (uncurry (+) battlers)

-- Tests ------------------------------------------------------------

getBattlers01 = (getBattlers (Battlefield 1 0)) == (0, 0)
getBattlers02 = (getBattlers (Battlefield 8 8)) == (3, 2)
getBattlers03 = (getBattlers (Battlefield 2 1)) == (1, 1)
getBattlers04 = (getBattlers (Battlefield 4 2)) == (3, 2)
getBattlersTests = [getBattlers01, getBattlers02, getBattlers03, getBattlers04]

resolve01 = (resolve (1, 1) ([3] ++ [6])) == (0, 1)
resolve02 = (resolve (2, 2) ([3,3] ++ [6,1])) == (1, 1)
resolve03 = (resolve (2, 2) ([3,3] ++ [3,3])) == (0, 2)
resolve04 = (resolve (3, 2) ([3,3,3] ++ [1,1])) == (3, 0)
resolve05 = (resolve (3, 2) ([1,1,1] ++ [3,3])) == (1, 2)
resolve06 = (resolve (1, 2) ([1] ++ [3,3])) == (0, 2)
resolve07 = (resolve (1, 2) ([6] ++ [3,3])) == (1, 1)
resolveTests = [resolve01, resolve02, resolve03, resolve04, resolve05, resolve06, resolve07]

instance Eq Battlefield where
  (==) a b = (attackers a == attackers b) && (defenders a == defenders b)

update01 = (update (Battlefield 5 5) (3, 0)) == Battlefield 5 3
update02 = (update (Battlefield 2 1) (0, 1)) == Battlefield 1 1
update03 = (update (Battlefield 3 1) (2, 0)) == Battlefield 3 0
updateTests = [update01, update02, update03]

instance Show Battlefield where
  show bf = "Attackers: " ++ show (attackers bf) ++ ", Defenders: " ++ show (defenders bf)

main :: IO()
main = do
  b <- evalRandIO (battle (Battlefield 8 8))
  print b
