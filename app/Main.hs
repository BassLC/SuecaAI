module Main where

import Game
import Rng
import RandomStrategy
import FirstCardStrategy

main :: IO ()
main = print (playGame game)
  where game = createNewGame players
        players = [createRandomPlayer, createFirstCardPlayer, createRandomPlayer, createFirstCardPlayer]
