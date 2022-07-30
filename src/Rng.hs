module Rng where

import Data.Bits
import Data.Word
import Data.List

-- Prelude does not have a RNG implementation
-- so we roll our own. Based on a simple LCG.
newtype Rng = Rng {rngState :: Word64} deriving (Show)
  
next :: Rng -> (Int, Rng)
next rng = (result, Rng newState)
-- Get higher order bits (we only need 32 bits), better entropy there
-- Constants lifted from:
-- https://nuclear.llnl.gov/CNP/rng/rngman/node4.html
  where result = fromIntegral $ shiftR (rngState rng) 32 
        mul = 2862933555777941757
        addend = 3037000493
        newState = rngState rng * mul + addend

generateRandomNumbers size rng
  | size <= 0 = ([], rng)
  | otherwise = (values, lastRng)
  where valuesWithRng = take size (iterate (\(_, nextRng) -> next nextRng) (next rng))
        lastRng = snd $ last valuesWithRng
        values = map fst valuesWithRng


-- Shuffle uses the RNG to generate a random index, and recursively
-- constructs a new list with elements from the previous one
shuffle :: Rng -> [a] -> ([a], Rng)
shuffle rng lst
  | null lst = ([], rng)
  | otherwise = (shuffledList, lastRng)
    where (randomValues, lastRng) = generateRandomNumbers (length lst) rng
          shuffledList = map snd $ sortBy (\(a, _) (b, _) -> compare a b) $ zip randomValues lst
       
chooseRandomly rng lst = (head shuffledLst, nextRng)
  where (shuffledLst, nextRng) = shuffle rng lst
