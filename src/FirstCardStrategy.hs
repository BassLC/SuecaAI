module FirstCardStrategy where

import Player

data FirstCard = FirstCard deriving (Show)

instance Strategy FirstCard where
  empty = FirstCard
  setup strat hand team _ id = Player strat hand team id
  giveCard _ player _ = (card, newPlayer)
    where card = head (playerHand player)
          newHand = tail (playerHand player)
          newPlayer = player {playerHand = newHand}

  updateWorldAfterRound _ player _ = player

