module FirstCardStrategy where

import           Data.List
import           Data.Maybe
import           Types

import           Game

setupFirstCard player _ = player { playerSetup = undefined,
                                   playerGiveCard = giveFirstCard,
                                   playerUpdateAfterRound = updateFirstCard}

giveFirstCard player round = head allowedHand
  where allowedHand = filterHandForAllowedCards round (playerHand player)
        choosenCard = head allowedHand

updateFirstCard player round = player {playerHand = updatedHand}
  where myCard = cardPlayed $ fromJust $ find ((== player) . playerOfPlay) round
        updatedHand = filter (/= myCard) (playerHand player)

createFirstCardPlayer :: PlayerConstructor
createFirstCardPlayer id hand team = Player id hand team setupFirstCard undefined undefined

