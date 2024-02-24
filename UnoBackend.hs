-- backend data for the uno
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}
module UnoBackend where
import HiddenDict
import Data.Maybe (isNothing, fromJust)

-- basic framework
data Action = Play Card | Draw
type Game = Action -> State -> Result
type Player = State -> Action
type PlayerID = Int
data Result = ContinueState State
    | EndGame [Double] State -- TEMP

-- current gameplay aura, draw deck, top discard, other discards, dictionary of player hands
-- number of players, current player, play direction
data State = State Aura Deck Card Deck (HiddenDict PlayerID Hand) Int PlayerID Int

-- collections of cards
type Hand = [Card] -- open
type Deck = [Card] -- hidden; needs to incorporate randomness
-- TODO: refactor Deck to a data type so that Show can be safely overridden
--instance Show Deck where
--    show :: Deck -> String
--    show d = "[Deck: " ++ show (length d) ++ "]"

-- card colour, card type
data Card = Card Int Int
instance Show Card where
    show :: Card -> String
    show (Card cn tn) = colString ++ typeString where
        colString
            | cn == 1 = "R"
            | cn == 2 = "Y"
            | cn == 3 = "G"
            | cn == 4 = "B"
            | otherwise = "W"
        typeString
            | tn == 10 = "<X>"
            | tn == 11 = "<=>"
            | tn == 12 = "+2"
            | tn == 13 = "ILD"
            | tn == 14 = "+4"
            | otherwise = show tn

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
data Aura = Aura Int Int Int

{-
0 = base (nothing is happening)
    0 k c = nothing special
1 = wild cards for card validity? can do other things
    1 k c = wild played, is colour c
2 = in draw 2 state, play a draw 2 or draw 4
    2 k c = you must draw k cards if nothing
3 = skip turn (for simplicity)
    3 k c = nothing special
4 = in draw 4 state, play a draw 4
    4 k c = you must draw k cards if nothing, wild is colour c

reverse turn is easy right now, no need to worry about it

these mostly depend on the top card, but there is extra information that would normally 
be vocalizd (+2, +4, wilds), or like skip, for simplicity
-}

first3 :: (a, b, c) -> a
first3 (a,b,c) = a
second3 :: (a, b, c) -> b
second3 (a,b,c) = b
third3 :: (a, b, c) -> c
third3 (a,b,c) = c

createDeckCol :: Int -> Deck
createDeckCol n = [Card n 00, Card n 01, Card n 01, Card n 02, Card n 02, Card n 03, Card n 03, Card n 04, Card n 04, Card n 05, Card n 05, Card n 06, Card n 06, Card n 07, Card n 07, Card n 08, Card n 08, Card n 09, Card n 09, Card n 10, Card n 10, Card n 11, Card n 11, Card n 12, Card n 12]
startingDeck :: Deck
startingDeck = [Card 0 13, Card 0 13, Card 0 13, Card 0 13, Card 0 14, Card 0 14, Card 0 14, Card 0 14] ++ createDeckCol 1 ++ createDeckCol 2 ++ createDeckCol 3 ++ createDeckCol 4

emptyHand :: Hand
emptyHand = []
emptyDeck :: Deck
emptyDeck = []

-- NOTE: replace instances of (Card 0 00) w/ card getter helper
-- remaining deck, starting top card, hand dictionary
dealCards :: Deck -> Int -> Int -> HiddenDict PlayerID Hand -> (Deck, Card, HiddenDict PlayerID Hand)
dealCards deck nplayers ncards dict = (fst dealtHands, Card 0 00, snd dealtHands) where
    dealtHands = dealCardsRound deck nplayers ncards dict

dealCardsRound :: Deck -> Int -> Int -> HiddenDict PlayerID Hand -> (Deck, HiddenDict PlayerID Hand)
dealCardsRound deck _ 0 dict = (deck, dict)
dealCardsRound deck nplayers ncards dict = dealCardsRound (fst newDeck) nplayers (ncards-1) (snd newDeck) where
    newDeck = dealCardsPlayer deck nplayers dict

dealCardsPlayer :: Deck -> Int -> HiddenDict PlayerID Hand -> (Deck, HiddenDict PlayerID Hand)
dealCardsPlayer deck 0 dict = (deck, dict)
dealCardsPlayer deck n dict =
    if isNothing currentHand then
        dealCardsPlayer (fst newDeck) (n-1) (insert n (snd newDeck : emptyHand) dict)
    else
        dealCardsPlayer (fst newDeck) (n-1) (update n (\ ch -> snd newDeck : fromJust ch) dict)
    where
        currentHand = get n dict
        newDeck = dealTopCard deck

-- TODO: figure out how to handle base case
dealTopCard :: Deck -> (Deck, Card)
dealTopCard [] = (emptyDeck, Card 0 00)
dealTopCard (c:d) = (d, c)

-- TODO: randomize
determineStartPlayer :: Int
determineStartPlayer = 0

-- TODO: implement algorithm
shuffle :: Deck -> Deck
shuffle deck = deck

-- SETUP STEPS:
    -- create starting deck
    -- deal cards to all players
        -- these form the hand dictionary
    -- determine starting player + order
    -- reveal top card
initWorld :: Int -> Int -> State
initWorld nplayers ncards = State (Aura 0 0 0) (first3 initData) (second3 initData) emptyDeck (third3 initData) nplayers determineStartPlayer 1 where
    initData = dealCards (shuffle startingDeck) nplayers ncards emptyDict

isCardPlayable :: Aura -> Card -> Card -> Bool
isCardPlayable (Aura state num col) (Card tcol tnum) (Card hcol hnum)
    | state == 4 && hnum == 14 = True
    | state == 2 && (hnum == 12 || hnum == 14) = True
    | state `elem` [2,4] = False
    | hcol == 0 = True
    | state == 1 && col == hcol = True
    | state == 1 = False
    | tnum == hnum = True
    | tcol == hcol = True
    | otherwise = False

endRound :: State -> State
endRound (State aura deck (Card col num) discard dict nplay currplay dir) = State (nextAura (Card col num) aura) deck (Card col num) discard dict nplay (fst dirplay) (snd dirplay) where
    dirplay = if num == 11 then ((currplay + (*) (-1) dir) `mod` nplay,(*) (-1) dir) else ((currplay + dir) `mod` nplay, dir)

nextAura (Card col num) aura = aura


-- draw deck, top discard, other discards, dictionary of player hands
-- number of players, current player, play direction
-- data State = State Deck Card Hand (HiddenDict PlayerID Hand) Int PlayerID Int

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
