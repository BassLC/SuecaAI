module RandomStrategy where

import Player
import Rng

newtype Random = Random {rng :: Rng}

instance Strategy Random where
  giveCard random player _ = (card, newPlayer)
    where (card, nextRng) = chooseRandomly (rng random) (playerHand player)
          newHand = filter (== card) (playerHand player)
          -- GADT is dumb so we can't just assign playerStrategy and playerHand
          -- we must assign them all
          newPlayer = Player {
            playerHand = newHand,
            playerTeam = playerTeam player,
            playerId = playerId player,
            playerStrategy = Random nextRng}

  updateWorldAfterRound _ player _ = player
  
