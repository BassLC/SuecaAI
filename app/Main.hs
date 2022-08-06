module Main where

import           FirstCardStrategy
import           Game
import           RandomStrategy
import           Rng

import           MonteCarloStrategy
import           Types

main :: IO ()
main = do
  mapM_ print (gamePlayers game)
  putStrLn ("Trump: " ++ show (gameTrumpCard game))
  mapM_ putStrLn roundsToPrint
  print points
  where game = createNewGame players
        players = [createMCPlayer (Rng 211202102910),
                   createRandomPlayer (Rng 67678676),
                   createMCPlayer (Rng 1212136473826478),
                   createRandomPlayer (Rng 897172917)]

        (finalState, points) = playGame game
        roundsToPrint = reverse $ map (printRound (cardSuit $ gameTrumpCard game))
                        (roundsPlayed finalState)

