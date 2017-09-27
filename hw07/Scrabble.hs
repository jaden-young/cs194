{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Data.Monoid
import Data.Char (toUpper)

-- * Exercise 3
--------------------------------------------------------------------------------

newtype Score = Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i


instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | x `elem` "AEILNORSTU" = 1
  | x `elem` "DG"         = 2
  | x `elem` "BCMP"       = 3
  | x `elem` "FHVWY"      = 4
  | x == 'K'               = 5
  | x `elem` "JX"         = 8
  | x `elem` "QZ"         = 10
    where x = toUpper c
score _ = 0

scoreString :: String -> Score
scoreString = foldr ((<>) . score) (Score 0)
