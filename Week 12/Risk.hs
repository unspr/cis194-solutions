{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Control.Monad.List
import Data.List

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

both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

dice :: Army -> Rand StdGen [Army]
dice n = replicateM n (getRandomR (1, 6)) >>= return.reverse.sort

getUnit :: Battlefield -> (Army, Army)
getUnit (Battlefield attack defend) = ((!!) (0:1:[1..]) attack, (!!) (0:1:repeat 2) defend)

battle :: Battlefield -> Rand StdGen Battlefield
battle b@(Battlefield attack defend) = do
      let (x, y) = both dice $ getUnit b
      ll <- x
      rr <- y
      let num = sum $ zipWith (\m n -> if m > n then 1 else 0) ll rr
      Battlefield <$> return (attack - ((min (length ll) (length rr)) - num)) <*> return (defend - num)


res = battle $ Battlefield 4 3
