module FirstCardStrategy where

import Types 
import Player
import Data.Maybe
import Data.List

import Game

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
                                           
