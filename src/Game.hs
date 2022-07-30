module Game where

import Rng
import Card
import Types
import Data.List
import Data.Maybe
import qualified Data.Map as Map

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

firstSuitOfRound round
  | null round = Nothing
  | otherwise = Just $ cardSuit $ cardPlayed $ head round
  
filterHandForAllowedCards round hand
  -- If we are first, or we don't have the Suit of the first card played
  -- then we are allowed to play whatever we want.
  | isNothing firstCardSuit || null filteredCards = hand
  -- Otherwise we must play a card with the same Suit of the first one.
  | otherwise = filteredCards
  where filteredCards = filter ((== fromJust firstCardSuit) . cardSuit) hand
        firstCardSuit = firstSuitOfRound round

instance Show GameState where
  show gamestate = undefined

isGameOver GameState {roundsPlayed = rounds} = length rounds == 10

playRound gameState = gameState {gamePlayers = updatedPlayers, roundsPlayed = round : roundsPlayed gameState}
  where players = gamePlayers gameState
        round = foldr (\player round -> let card = playerGiveCard player player round in Play card player : round) [] players
        updatedPlayers = map (\p -> playerUpdateAfterRound p p round) players
        

createNewGame :: [PlayerConstructor] -> GameState
createNewGame stratConstructors = GameState playersAfterSetup trumpCard (last players) []
  where (shuffledDeck, _) = shuffle (Rng 1271827182781782) generateDeck
        trumpCard = last shuffledDeck
        hands = generateHands shuffledDeck (length stratConstructors)
        constructorsWithData = zip4 stratConstructors [0..] hands (cycle [minBound ..])
        players = map (\(c, id, hand, team) -> c id hand team) constructorsWithData
        gameState = GameState players trumpCard (last players) []
        playersAfterSetup = map (\player -> playerSetup player player gameState) players
        

calculateScoreForRound trumpSuit round = (winningTeam, pointsOfRound round)
  where winningTeam = playerTeam $ playerOfPlay $ winningPlay trumpSuit round :: Team
        pointsOfRound = foldl (\points play -> points + valueOfPlay play) 0
        
calculateGameScore:: GameState -> Map.Map Team Int
calculateGameScore gameState = allPointsFromRounds
  where allRounds = roundsPlayed gameState
        startingPoints = Map.fromList $ zip [minBound ..] (repeat 0) :: Map.Map Team Int
        trump = cardSuit $ gameTrumpCard gameState
        allPointsFromRounds = foldl (\m round -> let (team, points) = calculateScoreForRound trump round in
                                        Map.update (\v -> Just $ v + points) team m) startingPoints allRounds
        
  
playGame gameState
  | isGameOver gameState = calculateGameScore gameState
  | otherwise = playGame (playRound gameState)

