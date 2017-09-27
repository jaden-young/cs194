{-# OPTIONS_GHC -fno-warn-orphans #-}
module Party where

import Employee
import Data.Tree
import Data.Monoid
import Data.List (sort)

-- * Exercise 1
--------------------------------------------------------------------------------

-- Description says not to worry about already being a member
-- or any other special cases
glCons :: Employee -> GuestList -> GuestList
glCons e (GL emps fun) = GL (e:emps) (empFun e + fun)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend a (GL e _) = foldr glCons a e

moreFun :: GuestList -> GuestList -> GuestList
moreFun g1@(GL _ f1) g2@(GL _ f2)
  | f1 < f2   = g2
  | otherwise = g1

-- * Exercise 2
--------------------------------------------------------------------------------

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (map (treeFold f) (subForest t))

-- * Exercise 3
--------------------------------------------------------------------------------

-- | Basically pure for employee -> GL
empToGL :: Employee -> GuestList
empToGL e = GL [e] (empFun e)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e [] = (empToGL e, mempty)
nextLevel e l = (maxWithE, maxWithoutE)
  where
    maxWithE    = foldr ((<>) . snd) (empToGL e) l
    maxWithoutE = foldr ((<>) . uncurry max) mempty l

-- * Exercise 4
--------------------------------------------------------------------------------

maxFun :: Tree Employee -> GuestList
maxFun t = uncurry max topLevel
  where topLevel = treeFold nextLevel t

-- * Exercise 5
--------------------------------------------------------------------------------

formatGL :: GuestList -> IO ()
formatGL (GL e f) = putStrLn $ "Total fun: " ++ show f ++ unlines (sort $ map empName e)


main :: IO ()
main = readFile "company.txt" >>= (formatGL . maxFun . read)
