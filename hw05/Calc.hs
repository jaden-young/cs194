{-# LANGUAGE FlexibleInstances #-}

module Calc where
import ExprT
import Parser
import Data.Maybe
import qualified Data.Map as M

-- Exercise 1
--------------------------------------------------------------------------------
eval :: ExprT -> Integer
eval (ExprT.Lit x) = x
eval (ExprT.Add x y) = (eval x) + (eval y)
eval (ExprT.Mul x y) = (eval x) * (eval y)

-- Exercise 2
--------------------------------------------------------------------------------
evalStr :: String -> Maybe Integer
evalStr s = let parsed = parseExp ExprT.Lit ExprT.Add ExprT.Mul s
                in case parsed of
                     Nothing -> Nothing
                     Just x -> Just (eval x)

-- Exercise 3
--------------------------------------------------------------------------------
class Expr a where
      lit :: Integer -> a
      add :: a -> a -> a
      mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (>0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax x) (MinMax y) = MinMax (max x y)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    lit = MinMax

instance Expr Mod7 where
    add (Mod7 x) (Mod7 y) = Mod7 ((x+y) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x*y) `mod` 7)
    lit = Mod7

reify :: ExprT -> ExprT
reify = id

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool    = testExp :: Maybe Bool
testMM      = testExp :: Maybe MinMax
testSat     = testExp :: Maybe Mod7

-- Exercise 6
--------------------------------------------------------------------------------

class HasVars a where
  var :: String -> a

data VarExprT = Lit Integer
              | Add VarExprT VarExprT
              | Mul VarExprT VarExprT
              | Var String

instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul

instance HasVars VarExprT where
  var = Var

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

instance Expr (M.Map String Integer -> Maybe Integer) where
  lit x _ = Just x
  add x y e
    | isNothing (x e) = Nothing
    | isNothing (y e) = Nothing
    | otherwise     = let (Just a) = x e
                          (Just b) = y e
                           in Just (a + b)
  mul x y e
    | isNothing (x e) = Nothing
    | isNothing (y e) = Nothing
    | otherwise     = let (Just a) = x e
                          (Just b) = y e
                           in Just (a * b)
withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs e = e $ M.fromList vs
