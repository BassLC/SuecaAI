module Player where

import Types

instance Show Player where
  show Player {playerHand = hand, playerTeam = team, playerId = id} =
    "Player: " ++ show id ++ "\tHand: " ++ show hand ++ "\tTeam: " ++ show team 

instance Eq Player where
  (==) player1 player2 = (==) (playerId player1) (playerId player2)

friend player1 player2 = playerTeam player1 == playerTeam player2
