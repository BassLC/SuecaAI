module FirstCardStrategy where

import Player
import Rng

data FirstCard = FirstCard

instance Strategy FirstCard where
  giveCard _ player _ = (card, newPlayer)
    where card = head (playerHand player)
          newHand = tail (playerHand player)
          newPlayer = player {playerHand = newHand}

  updateWorldAfterRound _ player _ = player
