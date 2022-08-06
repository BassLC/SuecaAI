module Game where

import           Data.List
import qualified Data.Map   as Map
import           Data.Maybe
import           Rng
import           Types

filterHandForAllowedCards round hand
  -- If we are first, or we don't have the Suit of the first card played
  -- then we are allowed to play whatever we want.
  | isNothing firstCardSuit || null filteredCards = hand
  -- Otherwise we must play a card with the same Suit of the first one.
  | otherwise = filteredCards
  where filteredCards = filter ((== fromJust firstCardSuit) . cardSuit) hand
        firstCardSuit = firstSuitOfRound round

-- instance Show GameState where
--   show gamestate = undefined

isGameOver GameState {roundsPlayed = rounds} = length rounds == 10

rotateLeft n ls = take (length ls) $ drop n (cycle ls)

playRound gameState = gameState {gamePlayers = updatedPlayers,
                                 roundsPlayed = round : roundsPlayed gameState}
  where players = gamePlayers gameState
        round = foldl' (\round player -> let card = playerGiveCard player gameState player round
                                        in round ++ [Play card player]) [] players
        indexPlayer = fromJust $ elemIndex (winningPlayer gameState round) players
        updatedPlayers = rotateLeft indexPlayer $
          map (\p -> playerUpdateAfterRound p gameState p round) players


createNewGame :: [PlayerConstructor] -> GameState
createNewGame stratConstructors = GameState playersAfterSetup trumpCard (last players) []
  where (shuffledDeck, _) = shuffle (Rng 1271827182781782) generateDeck
        trumpCard = last shuffledDeck
        hands = generateHands shuffledDeck (length stratConstructors)
        constructorsWithData = zip4 stratConstructors [0..] hands (cycle [minBound ..])
        players = map (\(c, id, hand, team) -> c id hand team) constructorsWithData
        gameState = GameState players trumpCard (last players) []
        playersAfterSetup = map (\player -> playerSetup player player gameState) players


calculateGameScore:: GameState -> Map.Map Team Int
calculateGameScore gameState = allPointsFromRounds
  where allRounds = roundsPlayed gameState
        startingPoints = Map.fromList $ zip [minBound ..] (repeat 0) :: Map.Map Team Int
        trump = cardSuit $ gameTrumpCard gameState
        allPointsFromRounds = foldr (\round m ->
                                       let (team, points) =
                                             calculateScoreForRound trump round in
                                         Map.update (\v -> Just $ v + points) team m)
                              startingPoints allRounds


playGame gameState
  | isGameOver gameState = (gameState, calculateGameScore gameState)
  | otherwise = playGame (playRound gameState)

