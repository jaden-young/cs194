module Hw04 where

-- Exercise 1
--------------------------------------------------------------------------------

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = foldr (\x acc -> (x - 2) * acc) 1 . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate calc
  where calc x = if even x then x `div` 2 else 3 * x + 1


-- Exercise 2
--------------------------------------------------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr treeInsert Leaf

treeInsert :: a -> Tree a -> Tree a
treeInsert x Leaf = Node 0 Leaf x Leaf
treeInsert x (Node _ l v r)
  | height l < height r = Node (height newL + 1) newL v r
  | otherwise           = Node (height newR + 1) l v newR
  where
    newL = treeInsert x l
    newR = treeInsert x r

height :: Tree a -> Integer
height Leaf = -1
height (Node h _ _ _) = h


-- Exercise 3
--------------------------------------------------------------------------------

-- I played around with lambdas for a shameful amount of time.
xor :: [Bool] -> Bool
xor = foldr (/=) False

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

-- Optional
--------------------------------------------------------------------------------

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (\x g y -> g (f y x)) id xs base

-- Exercise 4
--------------------------------------------------------------------------------

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

