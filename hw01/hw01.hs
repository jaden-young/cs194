module Hw01 where

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
  | x <= 0 = []
  | otherwise = (x `mod` 10) : toDigitsRev (x `div` 10)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther lst@(x:xs)
  | even $ length lst = (x * 2) : doubleEveryOther xs
  | otherwise = x : doubleEveryOther xs
doubleEveryOther a = a

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (sum . toDigits)

validate :: Integer -> Bool
validate x = summedDigits `mod` 10 == 0
  where summedDigits = sumDigits doubled
        doubled = doubleEveryOther $ toDigits x

-- Cool kids write point-free
validate' :: Integer -> Bool
validate' = (==) 0 . flip mod 10 . sumDigits . doubleEveryOther . toDigits

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = hanoi (n-1) a c b ++ [(a,b)] ++ hanoi (n-1) c b a
