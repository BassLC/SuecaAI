module Game where

import Rng
import Card
import Player
import Data.Ord
import Data.List
import Data.Maybe
import qualified Data.Map as Map

data GameState = GameState {
  gamePlayers :: [Player],
  gameTrumpCard :: Card,
  gameDealer :: Player,
  pointsForTeam :: Map.Map Team Int,
  startingAgentIndex :: Int}

isGameOver gameState = any (null . playerHand) (gamePlayers gameState)

createNewGame = shuffle rng deck
  where allSuits = [(minBound :: Suit) ..]
        allCardValues = [(minBound :: CardValue) ..]
        deck = [Card v s | v <- allCardValues, s <- allSuits]
        rng = snd $ next (Rng 122112908)
