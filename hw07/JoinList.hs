{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.List.Split (splitOn)
import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

-- * Exercise 1
--------------------------------------------------------------------------------

-- Damn monoids are sexy. I went way too far down into matching every
-- possible pattern before doing the right thing.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
x +++ y = Append (tag x <> tag y) x y

tag :: Monoid m => JoinList m a -> m
tag Empty          = mempty
tag (Single m _)   = m
tag (Append m _ _) = m

-- * Exercise 2
--------------------------------------------------------------------------------

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty       = Nothing
indexJ i jl
  | i < 0             = Nothing
  | i >= sizeToInt jl  = Nothing
indexJ _ (Single _ a) = Just a
indexJ i (Append _ l r)
  | i < s = indexJ i l
  | otherwise = indexJ (i - s) r
    where s = sizeToInt l

sizeToInt :: (Sized m, Monoid m) => JoinList m a -> Int
sizeToInt = getSize . size . tag

sing :: a -> JoinList Size a
sing = Single (Size 1)

myJL :: JoinList Size Char
myJL = Append (Size 4)
         (Append (Size 3)
           (Single (Size 1) 'y')
           (Append (Size 2)
             (Single (Size 1) 'e')
             (Single (Size 1) 'a')
           )
         )
         (Single (Size 1) 'h')

(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:_)  !!? 0         = Just x
(_:xs) !!? i         = xs !!? (i-1)

jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty = Empty
dropJ n jl
  | n < 0            = jl
  | n > sizeToInt jl = Empty
dropJ n jl@(Single _ _)
  | n <= 0 = jl
  | n > 0 = Empty
dropJ n (Append _ l r)
  | n < s     = dropJ n l +++ r
  | otherwise = dropJ (n - s) r
    where s = sizeToInt l

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty = Empty
takeJ n jl
  | n < 0 = Empty
  | n > sizeToInt jl = jl
takeJ n jl@(Single _ _)
  | n <= 0 = Empty
  | n > 0 = jl
takeJ n (Append _ l r)
  | n > s     = l +++ takeJ (n - s) r
  | otherwise = takeJ n l
    where s = sizeToInt l

-- * Exercise 3
--------------------------------------------------------------------------------

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

-- * Exercise 4
--------------------------------------------------------------------------------

singleLine :: String -> JoinList (Score, Size) String
singleLine s = Single (scoreString s, Size 1) s

instance Buffer (JoinList (Score, Size) String) where
  toString Empty = ""
  toString (Single (_, _) text) = text
  toString (Append (_, _) l r)  = toString l ++ "\n" ++ toString r

  fromString s  =  foldr ((+++) . singleLine) Empty $ lines s

  line = indexJ
  replaceLine n l b = takeJ n b +++ Single (scoreString l, Size 1) l +++ dropJ (n+1) b

  numLines = getSize . snd . tag
  value = getScore . fst . tag

main = runEditor editor $ singleLine "Omg this actually worked"
