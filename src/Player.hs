{-# LANGUAGE GADTs #-}
module Player where

import Card
import Data.List

data Play = Play {
  cardPlayed :: Card,
  playerOfPlay :: Player
}

comparePlay trump play1 play2
  -- If play2 is a trump suit and play1 is not, then play1 looses
  | isPlayTrump play2 && not (isPlayTrump play1) = LT
  -- Else, we have to compare the values of the cards
  | otherwise = compare (valuePlay play1) (valuePlay play2)
  where isPlayTrump play = cardSuit (cardPlayed play) == trump
        valuePlay play = valueOfCard $ cardPlayed play

winningPlay trump = maximumBy (comparePlay trump)

type Round = [Play]

data Team = Red | Blue  deriving (Read, Show, Eq)

-- Have to be pure and functional, so always return Player
-- as an updated copy of Player AFTER an action
class Strategy strat where
  -- `giveCard` HAS TO remove the card from the Player hand
  giveCard :: strat -> Player -> Round -> (Card, Player)
  updateWorldAfterRound :: strat -> Player -> Round -> Player

-- The `strat` type is any type that implements Strategy
data Player = forall strat. Strategy strat => Player {
  playerHand :: Hand,
  playerTeam :: Team,
  playerId :: Int,
  playerStrategy :: strat
}

instance Eq Player where
  (==) player1 player2 = (==) (playerId player1) (playerId player2)

friend player1 player2 = playerTeam player1 == playerTeam player2
