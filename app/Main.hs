module Main where

import           FirstCardStrategy
import           Game
import           RandomStrategy
import           Rng

import           Types

main :: IO ()
main = do
  mapM_ print (gamePlayers game)
  putStrLn ("Trump: " ++ show (gameTrumpCard game))
  mapM_ putStrLn roundsToPrint
  print points
  where game = createNewGame players
        players = [createRandomPlayer,
                   createFirstCardPlayer,
                   createRandomPlayer,
                   createFirstCardPlayer]

        (finalState, points) = playGame game
        roundsToPrint = reverse $ map (printRound (cardSuit $ gameTrumpCard game))
                        (roundsPlayed finalState)

