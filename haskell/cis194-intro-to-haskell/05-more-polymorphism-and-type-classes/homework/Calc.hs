{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Calc where

import ExprT
import Parser
import Data.Maybe
import qualified StackVM

-- Exercise 1 -------------------------------------------------------

-- Evaluates ExprT.
eval :: ExprT -> Integer
eval (Lit n)     = n
eval (Add e1 e2) = (+) (eval e1) (eval e2)
eval (Mul e1 e2) = (*) (eval e1) (eval e2)

-- Exercise 2 -------------------------------------------------------

-- Evaluates arithmetic expressions given as a String.
evalStr :: String -> Maybe Integer
evalStr s
  | isJust p  = Just (eval $ fromJust p)
  | otherwise = Nothing
  where p = parseExp Lit Add Mul s

-- Exercise 3 -------------------------------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit n     = Lit n
  add e1 e2 = Add e1 e2
  mul e1 e2 = Mul e1 e2

{-
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
  deriving (Show, Eq)
-}

-- Exercise 4 -------------------------------------------------------

-- Works like the original calculator.
instance Expr Integer where
  lit n     = n
  add e1 e2 = (+) e1 e2
  mul e1 e2 = (*) e1 e2

-- <= 0 is False, add is logical or, mul is logical and.
instance Expr Bool where
  lit n     = n > 0
  add e1 e2 = e1 || e2
  mul e1 e2 = e1 && e2

newtype MinMax = MinMax Integer deriving (Eq, Show, Ord)
newtype Mod7   = Mod7 Integer deriving (Eq, Show, Num, Integral, Enum, Real, Ord)

-- add is the max function, mul is the min function.
instance Expr MinMax where
  lit n     = MinMax n
  add e1 e2 = max e1 e2
  mul e1 e2 = min e1 e2

-- Everything is done mod 7.
instance Expr Mod7 where
  lit n     = Mod7 (mod n 7)
  add e1 e2 = (+) e1 e2 `mod` 7
  mul e1 e2 = (*) e1 e2 `mod` 7

-- Tests:
testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 5 -------------------------------------------------------

instance Expr StackVM.Program where
  lit n     = [StackVM.PushI n]
  add e1 e2 = [StackVM.Add] ++ e1 ++ e2
  mul e1 e2 = [StackVM.Mul] ++ e1 ++ e2

-- Compiles strings into programs to be run on the custom CPU in StackVM.
compile :: String -> Maybe StackVM.Program
compile s = parseExp lit add mul s

-- Exercise 6 (Optional) --------------------------------------------
