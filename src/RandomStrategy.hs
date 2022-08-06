module RandomStrategy where

import           Data.List
import           Data.Maybe
import           Game
import           Rng
import           Types

newtype RandomStrategy = RandomStrategy {rng :: Rng}

setupRandom rng player _ = player {playerSetup = undefined,
                               playerGiveCard = giveRandomCard startingRandom,
                               playerUpdateAfterRound = updateRandomAfterRound startingRandom }
                       where startingRandom = RandomStrategy {rng = rng}

giveRandomCard randomStrategy _ player round = cardChosen
  where (cardChosen, nextRng) = chooseRandomly (rng randomStrategy) allowedHand
        allowedHand = filterHandForAllowedCards round (playerHand player)

updateRandomAfterRound randomStrategy _ player round = player {playerHand = newHand, playerGiveCard = giveRandomCard nextStrategy}
  where cardChosen = cardPlayed $ fromJust $ find ((== player) . playerOfPlay) round
        newHand = filter (/= cardChosen) (playerHand player)
        (_, nextRng) = next (rng randomStrategy)
        nextStrategy = RandomStrategy nextRng


createRandomPlayer rng id hand team = Player id hand team (setupRandom rng) undefined undefined

