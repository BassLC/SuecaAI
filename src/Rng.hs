module Rng where

import Data.Bits
import Data.Word
import Data.List

-- Prelude does not have a RNG implementation
-- so we roll our own. Based on a simple LCG.
newtype Rng = Rng {rngState :: Word64}
  
next :: Rng -> (Int, Rng)
next rng = (result, Rng newState)
-- Get higher order bits (we only need 32 bits), better entropy there
-- Constants lifted from:
-- https://nuclear.llnl.gov/CNP/rng/rngman/node4.html
  where result = fromIntegral $ shiftR (rngState rng) 32 
        mul = 2862933555777941757
        addend = 3037000493
        newState = rngState rng * mul + addend

generateRandomNumberListWithRng size rng
  | size == 0 = []
  | otherwise = (n, nextRng) : generateRandomNumberListWithRng (size - 1) nextRng
  where (n, nextRng) = next rng
-- Shuffle uses the RNG to generate a random index, and recursively
-- constructs a new list with elements from the previous one
shuffle :: Rng -> [a] -> ([a], Rng)
shuffle rng lst
  | null lst = ([], rng)
  | otherwise = (shuffledList, lastRng)
    where (rnd, nextRng) = next rng
          randomValuesAndRng = generateRandomNumberListWithRng (length lst) rng
          randomValues = map fst randomValuesAndRng
          shuffledList = map snd $ sortBy (\(a, _) (b, _) -> compare a b) $ zip randomValues lst
          lastRng = snd $ last randomValuesAndRng
       

chooseRandomly rng lst = (head shuffledLst, nextRng)
  where (shuffledLst, nextRng) = shuffle rng lst
