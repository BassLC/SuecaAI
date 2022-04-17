{-# LANGUAGE FlexibleContexts #-}
module Game where

import Rng
import Card
import Player
import Data.List
import Data.Maybe
import qualified Data.Map as Map

  
data GameState = GameState {
  -- `gamePlayers` is ordered, the first Player is the first one to
  -- play, then the second one, so on and so on
  gamePlayers :: [Player],
  gameTrumpCard :: Card,
  gameDealer :: Player,
  pointsForTeam :: Map.Map Team Int
}

instance Show GameState where
  show GameState {gamePlayers = players,
                  gameTrumpCard = trumpCard,
                  gameDealer = dealer,
                  pointsForTeam = points} = "Game\nPlayers:\n" ++ playersStr ++
                                            "\nTrump Card:\t" ++ show trumpCard ++
                                            "\nDealer Id:\t" ++ show (playerId dealer) ++
                                            "\nPoints per Team:\t" ++ show points
    where playersStr = foldl (\str player -> str ++ show player ++ "\n") "" players

isGameOver gameState = any (null . playerHand) (gamePlayers gameState)

playRound gameState = gameState {
  gamePlayers = rotatedPlayers,
  pointsForTeam = updatedPoints
  }
  where roundPlayed = foldr (\player round ->
                                case player of Player {playerStrategy = st} ->
                                                 let (card, newPlayer) = giveCard st player round in
                                                   Play card newPlayer : round) [] (gamePlayers gameState)
        points = pointsOfRound roundPlayed
        trump = cardSuit $ gameTrumpCard gameState
        winningPlayer = playerOfPlay $ winningPlay trump roundPlayed
        updatedPoints = Map.update (\value -> Just (value + points)) (playerTeam winningPlayer) (pointsForTeam gameState)
        updatedPlayers = map (\play -> let player = playerOfPlay play in
                                case player of Player {playerStrategy = st} ->
                                                 updateWorldAfterRound st player roundPlayed) roundPlayed
        rotate lst n = drop n lst ++ take n lst
        rotatedPlayers = rotate updatedPlayers (fromJust $ elemIndex winningPlayer updatedPlayers)
        
  
createHands deck nHands
  | nHands == 1 = [take 10 deck]
  | otherwise = take 10 deck : createHands (drop 10 deck) (nHands - 1)
  


createNewGame strats = GameState {
  gamePlayers = players,
  gameTrumpCard = trumpCard,
  gameDealer = last players,
  pointsForTeam = startingPoints
  }
  where deck = fst $ shuffle rng [Card v s | v <- [minBound ..], s <- [minBound ..]]
        trumpCard = last deck
        rng = snd $ next (Rng 122112908)
        hands = createHands deck (length strats)
        players = map (\(strat, hand, team, id) -> setup strat hand team (cardSuit trumpCard) id)
                  $ zip4 strats hands (cycle [(minBound :: Team) ..]) [0..] 
        
        startingPoints = Map.fromList $ zip [(minBound :: Team) ..] (repeat 0)


playGame gameState
  | isGameOver gameState = teamPoints
  | otherwise = playGame (playRound gameState)
  where teamPoints = Map.toList (pointsForTeam gameState)

