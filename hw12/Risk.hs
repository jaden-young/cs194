{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Data.List
import Control.Monad
import Control.Monad.Random
import Data.Functor.Identity (runIdentity)

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

-- | Represents the outcome of one attacking unit fighting one defending unit
-- A if attacker wins, D if defender wins
data Winner = A | D
  deriving Eq


-- * Exercise 2
--------------------------------------------------------------------------------

battle :: Battlefield -> Rand StdGen Battlefield
battle (Battlefield a d) = do
  as <- rollN numAs
  ds <- rollN numDs
  let results = outcomes as ds
  return $ Battlefield (a - count D results) (d - count A results)
  where numAs = min 3 (a - 1)
        numDs = min 2 d

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die >>= \ds -> return $ sortBy (flip compare) ds

count :: (Eq a) => a -> [a] -> Int
count x = length . filter (==x)

fight :: DieValue -> DieValue -> Winner
fight a d = if a > d then A else D

outcomes :: [DieValue] -> [DieValue] -> [Winner]
outcomes = zipWith fight

-- * Exercise 3
--------------------------------------------------------------------------------

invade :: Battlefield -> Rand StdGen Battlefield
invade b
  | attackers b == 1 = return b
  | defenders b == 0 = return b
  | otherwise = battle b >>= invade

-- * Exercise 4
--------------------------------------------------------------------------------

successProb :: Battlefield -> Rand StdGen Double
successProb b = replicateM 1000 (invade b) >>= \rs ->
  return $ prob attackersWin rs
  where
    attackersWin x = attackers x > 1
    prob p xs = satisfied p xs / total xs
    satisfied p xs = fromIntegral $ length $ filter p xs :: Double
    total xs = fromIntegral $ length xs :: Double

sProb :: Int -> Int -> StdGen -> Double
sProb a d g = runIdentity $ evalRandT (successProb (Battlefield a d)) g
