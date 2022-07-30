module Types where

import Card

data Team = Red | Blue
  deriving (Read, Show, Eq, Ord, Bounded, Enum)

type Id = Int

data Player = Player {
  -- Basic data every Player should have
  playerId :: Id,
  playerHand :: Hand,
  playerTeam :: Team,

  -- These functions will be filled by each Strategy

  -- `playerSetup` is called after the GameState is created,
  -- but before any Round is played, 
  playerSetup :: Player -> GameState -> Player,

  -- `playerGiveCard` is called during a Round. It gives the current
  -- Player's state, the current Round's state, and expects a
  -- Card and an updated Player
  playerGiveCard :: Player -> Round -> Card,

  -- `playerUpdateAfterRound` allows the Player to check how the
  -- full Round went, and to update itself accordingly.
  playerUpdateAfterRound :: Player -> Round -> Player
}

data Play = Play {
  cardPlayed :: Card,
  playerOfPlay :: Player
} 

type Round = [Play]

data GameState = GameState {
  -- `gamePlayers` is ALWAYS ordered, the first Player is the first one to
  -- play, then the second one, so on and so on.
  -- The game starts as the the Dealer being the last player to play.
  gamePlayers :: [Player],
  gameTrumpCard :: Card,
  gameDealer :: Player,
  roundsPlayed :: [Round]
}

-- Each Strategy should implement a Constructor with this type
type PlayerConstructor = Id -> Hand -> Team -> Player 
