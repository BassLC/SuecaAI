module Main where

import Game
import RandomStrategy
import FirstCardStrategy
import Rng

main :: IO ()
main = print (playGame game)
  where game = createNewGame strats
        strats = [RandomStrategy (Rng 1812821), RandomStrategy (Rng 11021237131273612376), RandomStrategy (Rng 43126483671648716),
                  RandomStrategy (Rng 42673474)]
