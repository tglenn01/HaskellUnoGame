-- backend data for the uno
module UnoBackend where
import HiddenDict

-- basic framework
data Action = Play Card | Draw
type Game = Action -> State -> Result
type Player = State -> Action
type PlayerID = Int
data Result = ContinueState State
    | EndGame [Double] State -- TEMP

-- draw deck, top discard, other discards, dictionary of player hands
-- number of players, current player, play direction
data State = State Deck Card Hand (HiddenDict PlayerID Hand) Int PlayerID Int

-- collections of cards
data Hand = Hand [Card] -- open
data Deck = Deck [Card] -- hidden; needs to incorporate randomness

-- card colour, card type
data Card = Card Int Int
{- CARD MAPPING:
0 = black (wild)
1 = red
2 = yellow
3 = green
4 = blue

00 = 0
01 = 1
…
09 = 9
10 = skip
11 = reverse
12 = +2
13 = wild
14 = wild +4
-}
