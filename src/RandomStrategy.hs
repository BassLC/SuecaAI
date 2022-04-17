module RandomStrategy where

import Player
import Rng
import Debug.Trace (traceShow)

newtype RandomStrategy = RandomStrategy {rng :: Rng} 

instance Show RandomStrategy where
  show _ = "RandomStrategy"

instance Strategy RandomStrategy where
  empty = RandomStrategy {rng = Rng 1337}

  setup randomStrategy hand team _ id = Player (RandomStrategy newRng) hand team id
    where newRng = snd $ next $ rng randomStrategy

  giveCard randomStrategy player round = (card, newPlayer)
    where (card, nextRng) = chooseRandomly (rng randomStrategy) (allowedHand round (playerHand player))
          newHand = filter (/= card) (playerHand player)
          -- GADT is dumb so we can't just assign playerStrategy and playerHand
          -- we must assign them all
          newPlayer = Player {
            playerHand = newHand,
            playerTeam = playerTeam player,
            playerId = playerId player,
            playerStrategy = RandomStrategy nextRng}

  updateWorldAfterRound _ player _ = player
  
