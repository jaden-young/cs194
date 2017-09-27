module Golf where

import Data.List

-- * Exercise 1
--------------------------------------------------------------------------------

{-| skips @xs@ takes  a list xs and returns a new list where
   every ith item is a list formed by keeping every ith element
   from the original @xs@.
-}
skips :: [a] -> [[a]]
skips xs = map (skipN xs) [1..length xs]

{-| skipN @xs@ @n@ takes a list @xs@ and returns a new list
    where every @n@th item is kept.
    It works by first assigning an index to each value by creating
    a tuple of the form (i,x) where i is the index and x the value.
    Then to determine whether or not to keep that ith item, we check
    that the value i + n is divisible by n.

    >>> skipN "abcd" 1
    "abcd"

    >>> skipN "abcd" 2
    "bd"

    >>> skipN "abcd" 3
    "c"
-}
skipN :: (Integral b) => [a] -> b -> [a]
skipN xs n = [ x | (i, x) <- zip [1..] xs, (i + n) `mod` n == 0]

-- * Exercise 2
--------------------------------------------------------------------------------

{-| localMaxima takes a list and returns a list of its local maxima, defined as
an element that is strictly greater than both the previous and next elements.

  Algorithm example: @[2,9,5,6,1]@

  1. Consider a list of 3-element sequences from the given list

     > [(2,9,5),(9,5,6),(5,6,1)]

  2. Filter this list, keeping triples that have a local maximum

     > [(2,9,5),(5,6,1)]

  3. Extract the 2nd element from each of these triples

     > [9,6]
-}
localMaxima :: [Integer] -> [Integer]
localMaxima = map snd3
              . filter isLocalMaximum
              . groupBy3

groupBy3 :: [a] -> [(a,a,a)]
groupBy3 [] = []
groupBy3 [_] = []
groupBy3 [_,_] = []
groupBy3 l@(_:x:xs) = zipWith3 (\ a b c -> (a,b,c)) l (x:xs) xs

isLocalMaximum :: Ord a => (a,a,a) -> Bool
isLocalMaximum (a,b,c) = b > a && b > c

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

-- * Exercise 3
--------------------------------------------------------------------------------
{-| histogram takes a list of integers 0-9 and returns a textual representation
of a vertical histogram, with a star per ocurrance of the digit.

Algorithm:

  Ex. given @[1,1,5,6]@

  1. Make a horizontal histogram

    * Count the number of ocurrances of each digit in a new list with
       corresponding indicies

       @[0,2,0,0,0,1,1,0,0,0]@

    * Replace these counts with a string representation of "*"s

       @[""," * *","","",""," *"," *","","",""]@

    * Pad with whitespace so we get a square matrix

       @
       ["    "," * *","    ","    ","    "," *  "," *  ","    ","    ","    "]
          ^      ^      ^      ^      ^      ^      ^      ^      ^      ^
          0      1      2      3      4      5      6      7      8      9
       @

    * Add labels to each of these

       @
       ["0=    ","1= * *","2=    ","3=    ","4=    "
       ,"5= *  ","6= *  ","7=    ","8=    ","9=    "
       ]
       @

  2. Transform horizontal histogram to vertical

-}

histogram :: [Int] -> String
histogram = makeVertical . horizontalHistogram
  where makeVertical = intercalate "\n" . reverse . transpose

horizontalHistogram :: [Int] -> [String]
horizontalHistogram = addLabels
                      . makeSquare
                      . map starify
                      . counts [0..9]

-- | Count the occurances of @x@ in a list
count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

-- | Similar to count, but instead takes a list of items to search for.
-- The result is a list of counts, with indecies corresponding to the index
-- of elementes from @countThese@
counts :: Eq a => [a] -> [a] -> [Int]
counts countThese inThis = map (flip count inThis) countThese

starify :: Int -> String
starify n = take (n * 2) $ cycle [' ', '*']

addLabels :: [String] -> [String]
addLabels = zipWith (\lbl x -> (show lbl) ++ "=" ++ x) [0..9]

makeSquare :: [String] -> [String]
makeSquare xs = map (take maxWidth) paddedRows
  where
    maxWidth = maximum $ map length xs
    paddedRows = map (++ repeat ' ') xs
