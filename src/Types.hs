module Types where

import           Data.List
import           Data.Maybe

data Team = Red | Blue
  deriving (Read, Show, Eq, Ord, Bounded, Enum)

enemyTeam Red  = Blue
enemyTeam Blue = Red

data Suit = Heart | Club | Diamond | Spade
          deriving (Read, Enum, Eq, Bounded, Ord)

instance Show Suit where
  show suit = case suit of
                Heart   -> "❤"
                Club    -> "♣"
                Diamond -> "◆"
                Spade   -> "♠"


data CardValue = Two | Three | Four | Five | Six | Queen | Jack | King | Seven | Ace
               deriving (Read, Enum, Eq, Bounded, Ord)

instance Show CardValue where
  show value = case value of
                 Two   -> "2"
                 Three -> "3"
                 Four  -> "4"
                 Five  -> "5"
                 Six   -> "6"
                 Seven -> "7"
                 Queen -> "Q"
                 Jack  -> "J"
                 King  -> "K"
                 Ace   -> "A"

data Card = Card {
  cardSuit  :: Suit,
  cardValue :: CardValue
} deriving (Read, Eq, Ord)

instance Show Card where
  show card = show (cardValue card) ++ show (cardSuit card)

valueOfCard card = case cardValue card of
                 Queen -> 2
                 Jack  -> 3
                 King  -> 4
                 Seven -> 10
                 Ace   -> 11
                 _     -> 0

generateDeck = [Card v s | v <- [minBound ..], s <- [minBound ..]]

type Hand = [Card]

generateHands deck nHands
  | nHands == 0 = []
  | nHands == 1 = [take 10 deck]
  | otherwise = take 10 deck : generateHands (drop 10 deck) (nHands - 1)

type Id = Int

data Player = Player {
  -- Basic data every Player should have
  playerId               :: Id,
  playerHand             :: Hand,
  playerTeam             :: Team,

  -- These functions will be filled by each Strategy

  -- `playerSetup` is called after the GameState is created,
  -- but before any Round is played,
  playerSetup            :: Player -> GameState -> Player,

  -- `playerGiveCard` is called during a Round. It gives the current
  -- Player's state, the current Round's state, and expects a
  -- Card
  playerGiveCard         :: GameState -> Player -> Round -> Card,

  -- `playerUpdateAfterRound` allows the Player to check how the
  -- full Round went, and to update itself accordingly.
  playerUpdateAfterRound :: GameState -> Player -> Round -> Player
}

instance Ord Player where
  compare p1 p2 = compare (playerId p1) (playerId p2)

instance Show Player where
  show Player {playerHand = hand, playerTeam = team, playerId = id} =
    "Player: " ++ show id ++ "\tHand: " ++ show hand ++ "\tTeam: " ++ show team

instance Eq Player where
  (==) player1 player2 = (==) (playerId player1) (playerId player2)

friend player1 player2 = playerTeam player1 == playerTeam player2

data Play = Play {
  cardPlayed   :: Card,
  playerOfPlay :: Player
} deriving (Eq)

instance Show Play where
  show play = "P" ++ id ++ "|" ++ team ++ " -> " ++ show (cardPlayed play)
    where id = show $ playerId $ playerOfPlay play
          team = show $ playerTeam $ playerOfPlay play

valueOfPlay = valueOfCard . cardPlayed

sortPlay trump firstSuit play1 play2
  -- Handle the case where one of them is the trump and the other is not
  | playIsTrump play1 && not (playIsTrump play2) = GT
  | playIsTrump play2 && not (playIsTrump play1) = LT
  -- Then, handle the case where one of them is the first played Suit and
  -- the other is not
  | playIsFirstSuit play1 && not (playIsFirstSuit play2) = GT
  | playIsFirstSuit play2 && not (playIsFirstSuit play1) = LT
  -- In case both are of the first played Suit, then we must compare their Card
  -- values
  | playIsFirstSuit play1 && playIsFirstSuit play2 = compare (valueOfPlay play1) (valueOfPlay play2)
  -- Finally, they are neither a trump or a Card with the first played Suit, so they
  -- equally shit
  | otherwise = EQ
  where playHasSuit suit play = (== suit) $ cardSuit $ cardPlayed play
        playIsTrump = playHasSuit trump
        playIsFirstSuit = playHasSuit firstSuit

-- Winning play is the maximum of all Plays in a Round
winningPlay trump round = maximumBy (sortPlay trump (fromJust $ firstSuitOfRound round)) round

winningPlayer gameState round = playerOfPlay $
  winningPlay (cardSuit $ gameTrumpCard gameState) round

-- `Round` is in order of plays:
-- `head round` is the first play made in the round
type Round = [Play]

firstSuitOfRound round
  | null round = Nothing
  | otherwise = Just $ cardSuit $ cardPlayed $ head round

calculateScoreForRound :: Suit -> Round -> (Team, Int)
calculateScoreForRound trumpSuit round = (winningTeam, pointsOfRound round)
  where winningTeam = playerTeam $ playerOfPlay $ winningPlay trumpSuit round :: Team
        pointsOfRound = foldr (\play points -> points + valueOfPlay play) 0

printRound trumpSuit round
  | length round == 4 = show round ++ " -> " ++ show score
  | otherwise = show round
  where score = calculateScoreForRound trumpSuit round

data GameState = GameState {
  -- `gamePlayers` is ALWAYS ordered, the first Player is the first one to
  -- play, then the second one, so on and so on.
  -- The game starts as the the Dealer being the last player to play.
  gamePlayers   :: [Player],
  gameTrumpCard :: Card,
  gameDealer    :: Player,
  -- `roundsPlayed` is in INVERSE order: the `head roundsPlayed` is the
  -- last round played.
  roundsPlayed  :: [Round]
} deriving (Show)

-- Each Strategy should implement a Constructor with this type
type PlayerConstructor = Id -> Hand -> Team -> Player
