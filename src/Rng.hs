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

generateRandomNumberList:: Rng -> Int -> [Int]
generateRandomNumberList rng size
  | size == 0 = []
  | otherwise = res : generateRandomNumberList newRng (size - 1)
  where (res, newRng) = next rng
  

-- Shuffle uses the RNG to generate a random list of numbers, then assigns each member
-- of the given list to one of these numbers. It sorts the list (only by the random
-- number given, so the general type does not have to implement Ord). Finally, it removes
-- the random numbers given and returns only the starting list, with a now random order
shuffle :: Rng -> [a] -> [a]
shuffle rng lst = map snd $ sortBy (\(a, _) (b, _) -> compare a b) $ zip (generateRandomNumberList rng size) lst
  where size = length lst
        rndNumberList = generateRandomNumberList rng size

-- Yeah yeah it is not fully random, go suck a lemon
chooseRandomly rng lst = (lst !! index, nextRng)
  where (n, nextRng) = next rng
        index = mod n (length lst)
