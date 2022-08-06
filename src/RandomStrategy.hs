module RandomStrategy where

import           Data.List
import           Data.Maybe
import           Game
import           Rng
import           Types

newtype RandomStrategy = RandomStrategy {rng :: Rng}

setupRandom player _ = player {playerSetup = undefined,
                               playerGiveCard = giveRandomCard startingRandom,
                               playerUpdateAfterRound = updateRandomAfterRound startingRandom }
                       where startingRandom = RandomStrategy (Rng 101010101010)

giveRandomCard randomStrategy player round = cardChosen
  where (cardChosen, nextRng) = chooseRandomly (rng randomStrategy) allowedHand
        allowedHand = filterHandForAllowedCards round (playerHand player)

updateRandomAfterRound randomStrategy player round = player {playerHand = newHand, playerGiveCard = giveRandomCard nextStrategy}
  where cardChosen = cardPlayed $ fromJust $ find ((== player) . playerOfPlay) round
        newHand = filter (/= cardChosen) (playerHand player)
        (_, nextRng) = next (rng randomStrategy)
        nextStrategy = RandomStrategy nextRng


createRandomPlayer :: PlayerConstructor
createRandomPlayer id hand team = Player id hand team setupRandom undefined undefined

