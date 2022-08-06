module MonteCarloStrategy where

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import FirstCardStrategy (createFirstCardPlayer, setupFirstCard)
import Game
import Rng
import Types

type MCReward = Float

data MCNode = MCNode
  { nodeReward :: MCReward,
    nodeNVisited :: Int,
    nodeAction :: Play
  }
  deriving (Eq, Show)

calculateUCB1 :: Int -> MCNode -> MCReward
calculateUCB1 nIterations node
  | nVisited == 0 = 1 / 0
  | otherwise = averageReward + sqrt sqrtFactor
  where
    averageReward = nodeReward node / nVisited
    nVisited = fromIntegral $ nodeNVisited node :: Float
    dividend = 2.0 * log (fromIntegral nIterations) :: Float
    sqrtFactor = dividend / nVisited :: Float

setupMCPlayer :: Rng -> Player -> GameState -> Player
setupMCPlayer rng player gameState =
  player
    { playerSetup = undefined,
      playerGiveCard = giveMCCard rng 10000,
      playerUpdateAfterRound = updateMC rng
    }

giveMCCard :: Rng -> Int -> GameState -> Player -> Round -> Card
giveMCCard rng nMaxIterations gameState player round = cardPlayed $ nodeAction selectedNode
  where
    allPossiblePlays =
      map
        (`Play` player)
        ( filterHandForAllowedCards
            round
            (playerHand player)
        ) ::
        [Play]
    possibleNodes = map (MCNode 0 0) allPossiblePlays
    selectedNode = selectMCNode rng 0 nMaxIterations gameState round possibleNodes

selectMCNode :: Rng -> Int -> Int -> GameState -> Round -> [MCNode] -> MCNode
selectMCNode rng nIterations maxIterations gameState round nodes
  | length nodes == 1 = head nodes
  | nIterations >= maxIterations = selectedNode
  | otherwise = selectMCNode nextRng (nIterations + 1) maxIterations gameState round nextIteration
  where
    selectedNode =
      maximumBy
        ( \n1 n2 ->
            compare
              (calculateUCB1 nIterations n1)
              (calculateUCB1 nIterations n2)
        )
        nodes
    nextIteration = simulateMC rng gameState round nodes selectedNode
    nextRng = snd $ next rng

simulateMC :: Rng -> GameState -> Round -> [MCNode] -> MCNode -> [MCNode]
simulateMC rng gameState round previousNodes selectedNode = replaceNode previousNodes selectedNode newNode
  where
    reward = rolloutMC rng gameState round selectedNode
    newNode =
      selectedNode
        { nodeReward = nodeReward selectedNode + reward,
          nodeNVisited = nodeNVisited selectedNode + 1
        }
    replaceNode nodes previousNode newNode = newNode : filter (/= previousNode) nodes

rolloutMC :: Rng -> GameState -> Round -> MCNode -> MCReward
rolloutMC rng gameState round selectedNode = pointsToValue (ourPoints - othersPoints)
  where
    rolloutResult = snd $ playGame rolloutGameState
    ourTeam = playerTeam $ playerOfPlay $ nodeAction selectedNode :: Team
    (ourPoints, othersPoints) =
      ( rolloutResult Map.! ourTeam,
        rolloutResult Map.! enemyTeam ourTeam
      ) ::
        (Int, Int)
    rolloutGameState = createPossibleGameState rng gameState round selectedNode

createPossibleGameState rng gameState round selectedNode =
  gameState
    { gamePlayers = possiblePlayers,
      roundsPlayed = possibleRound : roundsPlayed gameState
    }
  where
    me = playerOfPlay (nodeAction selectedNode)
    playersThatArentMe = map fst $ sortBy (flip (\(_, suits1) (_, suits2) -> compare (length suits1) (length suits2))) (map (\player -> (player, generateSuitsPlayerDoesNotHave gameState round player)) $ filter (/= me) (gamePlayers gameState)) :: [Player]
    (myHandAfterRound, nextRng) = shuffle rng $ filter (\card -> card /= cardPlayed (nodeAction selectedNode)) (playerHand me)

    handsForOtherPlayersInThisRound = generatePossibleHandsForPlayers nextRng gameState round playersThatArentMe (getRemainingCards gameState round) :: [(Player, [Card])]
    remainingCards = Set.difference (getRemainingCards gameState round) (Set.fromList (playerHand me))
    restOfRound =
      map fromJust $ filter isJust $ map
          ( \(player, hand) ->
              if length hand > length myHandAfterRound
                then Just (Play (head $ filterHandForAllowedCards round hand) player)
                else Nothing
          ) handsForOtherPlayersInThisRound
    handsForOtherPlayersAfterRound = map (\(player, hand) ->
                                            let cardsInRestOfRound = map cardPlayed restOfRound in
                                              (player, filter (`notElem` cardsInRestOfRound) hand)) handsForOtherPlayersInThisRound
    possiblePlayers = map (\(player, hand) ->
                             setupFirstCard (createFirstCardPlayer (playerId player)
                                             hand
                                             (playerTeam player)) gameState)
                      ((me, myHandAfterRound) : handsForOtherPlayersAfterRound)
    possibleRound = round ++ [nodeAction selectedNode] ++ restOfRound

getRemainingCards gameState round = Set.difference allCards playedCards
  where
    playedCards = Set.fromList $ concatMap cardsFromRound allRounds :: Set.Set Card
    allRounds = round : roundsPlayed gameState
    cardsFromRound = map cardPlayed
    allCards = Set.fromList generateDeck

generatePossibleHandsForPlayers rng gameState round [] remainingCards = []
generatePossibleHandsForPlayers rng gameState round players remainingCards =
  (player, hand) :
  generatePossibleHandsForPlayers
    nextRng
    gameState
    round
    (tail players)
    nextRemainingCards
  where
    player = head players
    (hand, nextRng) = generatePossibleHandForPlayer rng gameState round player remainingCards
    nextRemainingCards = Set.difference remainingCards (Set.fromList hand)

generatePossibleHandForPlayer rng gameState round player remainingCards = (take possibleHandSize possibleCards, nextRng)
  where
    possibleHandSize = if hasPlayerPlayedInRound then 9 - fullRoundsPlayed else 10 - fullRoundsPlayed
    (possibleCards, nextRng) = shuffle rng $ generatePossibleCardsForPlayer gameState round player (generateSuitsPlayerDoesNotHave
                                                                                                    gameState round player) remainingCards

    hasPlayerPlayedInRound = any ((== player) . playerOfPlay) round
    fullRoundsPlayed = length (roundsPlayed gameState)

generatePossibleCardsForPlayer gameState round player suitsPlayerDoesNotHave remainingCards
  | player == gameDealer gameState = Set.toList possibleCards
  | otherwise = Set.toList $ Set.delete (gameTrumpCard gameState) possibleCards
  where
    possibleCards =
      Set.filter
        ( \card ->
            cardSuit card
              `notElem` suitsPlayerDoesNotHave
        )
        remainingCards

generateSuitsPlayerDoesNotHave gameState round player = suitsPlayerDoesNotHave
    where suitsFirstPlayedInEachRound = map (cardSuit . cardPlayed . head) allRounds
          allRounds = round : roundsPlayed gameState
          suitPlayedInEachRound =
            map ( \r -> let ourPlaysInARound = filter ( (== player) . playerOfPlay) r in
                    if null ourPlaysInARound
                    then Nothing
                    else Just (cardSuit $ cardPlayed $ head ourPlaysInARound)
            ) allRounds :: [Maybe Suit]
          suitsPlayerDoesNotHave = foldr ( \(suitPlayed, firstSuit) suits ->
                                             if isNothing suitPlayed || fromJust suitPlayed == firstSuit
                                             then suits
                                             else Set.insert firstSuit suits
                                         ) Set.empty $ zip suitPlayedInEachRound suitsFirstPlayedInEachRound

pointsToValue :: Int -> MCReward
pointsToValue points
  | points >= 60 = 1.0
  | points <= -60 = -1.0
  | otherwise = fromIntegral points / 60

-- Update Player's GameState for next iteration
updateMC :: Rng -> GameState -> Player -> Round -> Player
updateMC rng gameState player round =
  player
    { playerHand = updatedHand,
      playerGiveCard = giveMCCard nextRng 10000,
      playerUpdateAfterRound = updateMC nextRng
    }
  where
    card = cardPlayed $ head $ filter ((== player) . playerOfPlay) round
    updatedHand = filter (/= card) $ playerHand player
    nextRng = snd $ next rng


createMCPlayer rng id hand team = Player id hand team (setupMCPlayer rng) undefined undefined
