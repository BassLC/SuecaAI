module Card where

data Suit = Heart | Club | Diamond | Spade
          deriving (Read, Enum, Eq, Bounded)

instance Show Suit where
  show suit = case suit of
                Heart -> "❤"
                Club -> "♣"
                Diamond -> "◆"
                Spade -> "♠"


data CardValue = Two | Three | Four | Five | Six | Queen | Jack | King | Seven | Ace
               deriving (Read, Enum, Eq, Bounded)

instance Show CardValue where
  show value = case value of
                 Two -> "2"
                 Three -> "3"
                 Four -> "4"
                 Five -> "5"
                 Six -> "6"
                 Seven -> "7"
                 Queen -> "Q"
                 Jack -> "J"
                 King -> "K"
                 Ace -> "A"

data Card = Card {cardValue :: CardValue,
                  cardSuit :: Suit}
            deriving (Read, Eq)

instance Show Card where
  show card = show (cardValue card) ++ show (cardSuit card)

valueOfCard card = case cardValue card of
                 Queen -> 2
                 Jack -> 3
                 King -> 4
                 Seven -> 10
                 Ace -> 11
                 _ -> 0

type Hand = [Card]
