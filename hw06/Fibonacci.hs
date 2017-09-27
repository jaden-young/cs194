{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

-- * Exercise 1
--------------------------------------------------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

-- * Exercise 2
--------------------------------------------------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

-- * Exercise 3
--------------------------------------------------------------------------------

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons x y) = x : streamToList y

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

-- * Exercise 4
--------------------------------------------------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x y) = Cons (f x) (streamMap f y)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamMap f $ streamFromSeed f x)

-- * Exercise 5
--------------------------------------------------------------------------------

nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x y) z = Cons x (interleaveStreams z y)

ruler :: Stream Integer
ruler = foldr (interleaveStreams . streamRepeat) (streamRepeat 0) [0..]

-- * Exercise 6
-- Spent too much time trying to figure this out, exponentiation doesn't work.
--------------------------------------------------------------------------------

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons a as) (Cons b bs) = Cons (a + b) (as + bs)
  (*) (Cons a as) (Cons b bs) = Cons (a*b) (streamRepeat 0) + Cons 0 (streamMap (*a) bs + streamMap (*b) as)


-- * Exercise 7
--------------------------------------------------------------------------------

data Matrix = Matrix Integer Integer Integer Integer

instance Num Matrix where
  (*) (Matrix a1 b1 c1 d1) (Matrix a2 b2 c2 d2) = Matrix a b c d
    where a = (a1 * a2) + (b1 * c2)
          b = (a1 * b2) + (b1 * d2)
          c = (c1 * a2) + (d1 * c2)
          d = (c1 * b2) + (d1 * d2)

instance Show Matrix where
  show (Matrix a b c d) = show a ++ " " ++ show b ++ "\n" ++ show c ++ " " ++ show d

fibMatrix :: Matrix
fibMatrix = Matrix 1 1 1 0

fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = (\(Matrix _ f _ _) -> f) $ fibMatrix^n

