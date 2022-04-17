{-# LANGUAGE ExistentialQuantification #-}
module Player where

import Card
import Data.List

data Team = Red | Blue  deriving (Read, Show, Eq, Ord, Bounded, Enum)

-- The `a` type is any type that implements Strategy
data Player = forall a. (Strategy a, Show a) => Player {
  playerStrategy :: a,
  playerHand :: Hand,
  playerTeam :: Team,
  playerId :: Int
}

instance Show Player where
  show Player {playerStrategy = st, playerHand = hand, playerTeam = team, playerId = id} =
    "Player: " ++ show id ++ "\tHand: " ++ show hand ++ "\tTeam: " ++ show team ++ "\tStrategy: " ++ show st

instance Eq Player where
  (==) player1 player2 = (==) (playerId player1) (playerId player2)

friend player1 player2 = playerTeam player1 == playerTeam player2

data Play = Play {
  cardPlayed :: Card,
  playerOfPlay :: Player
}

valueOfPlay play = valueOfCard $ cardPlayed play

comparePlay trump play1 play2
  -- If play2 is a trump suit and play1 is not, then play1 looses
  | isPlayTrump play2 && not (isPlayTrump play1) = LT
  -- Else, we have to compare the values of the cards
  | otherwise = compare (valueOfPlay play1) (valueOfPlay play2)
  where isPlayTrump play = cardSuit (cardPlayed play) == trump

winningPlay trump = maximumBy (comparePlay trump)

type Round = [Play]

pointsOfRound :: Round -> Int
pointsOfRound = foldl (\points play -> points + valueOfPlay play) 0

-- Have to be pure and functional, so always return Player
-- as an updated copy of Player AFTER an action
class Strategy strat where
  empty :: strat
  setup :: strat -> Hand -> Team -> Suit -> Int -> Player
  -- `giveCard` HAS TO remove the card from the Player hand
  giveCard :: strat -> Player -> Round -> (Card, Player)
  updateWorldAfterRound :: strat -> Player -> Round -> Player

allowedHand round hand
  | null round || null filteredCards = hand
  | otherwise = filteredCards
  where filteredCards = filter ((== firstCardSuit) . cardSuit) hand
        firstCardSuit = cardSuit $ cardPlayed $ head round

