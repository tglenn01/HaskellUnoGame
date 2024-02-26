-- backend data for the uno
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use uncurry" #-}

module UnoBackend where
import HiddenDict
import Text.Read (readMaybe)
import Data.Maybe (isNothing, isJust, fromJust)

-- NOTE: unused definitions
type Game = Action -> State -> Result
type Player = State -> Action
data Result = ContinueState State
    | EndGame [Double] State -- TEMP

-- basic framework
-- card played, wild colour chosen (if applicable), UNO called
data Action = Play Card Int Bool | Draw
type PlayerID = Int

-- current gameplay aura, draw deck, top discard, other discards, dictionary of player hands
-- number of players, current player, play direction
data State = State Aura Deck Card Deck (HiddenDict PlayerID [Card]) Int PlayerID Int
-- NOTE: make sure draw deck is not shown in state

-- Rules == Stack Draw 2, Draw 4, Swap hands on 7, Rotate Hands on 0, Draw until play, Number of start decks
data Rules = Rules Bool Bool Bool Bool Bool Int

-- collections of cards
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
instance Eq Card where
    (==) :: Card -> Card -> Bool
    (Card c1 n1) == (Card c2 n2) = c1 == c2 && n1 == n2
instance Ord Card where
    (<=) :: Card -> Card -> Bool
    (Card c1 n1) <= (Card c2 n2)
        | c1 /= c2 = c1 < c2
        | otherwise = n1 <= n2

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
instance Show Aura where
    show :: Aura -> String
    show (Aura state num col)
        | state == 1 = " [" ++ stringifyCol col ++ "]"
        | state == 2 = " [+" ++ show num ++ "]"
        | state == 3 = " [turn skipped]"
        | state == 4 = " [" ++ stringifyCol col ++ ", +" ++ show num ++ "]"
        | otherwise = ""

stringifyCol :: Int -> String
stringifyCol col
    | col == 1 = "red"
    | col == 2 = "yellow"
    | col == 3 = "green"
    | col == 4 = "blue"
    | otherwise = "black"

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
be vocalized (+2, +4, wilds), or like skip, for simplicity
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

emptyHand :: [Card]
emptyHand = []
emptyDeck :: Deck
emptyDeck = []

-- remaining deck, starting top card, hand dictionary
dealCards :: Deck -> Int -> Int -> HiddenDict PlayerID [Card] -> (Deck, Card, HiddenDict PlayerID [Card])
dealCards deck nplayers ncards dict = (first3 topCard, second3 topCard, snd dealtHands) where
    dealtHands = dealCardsRound deck (nplayers-1) ncards dict
    topCard = drawTopCard (fst dealtHands) []

dealCardsRound :: Deck -> Int -> Int -> HiddenDict PlayerID [Card] -> (Deck, HiddenDict PlayerID [Card])
dealCardsRound deck _ 0 dict = (deck, dict)
dealCardsRound deck nplayers ncards dict = dealCardsRound (fst newDeck) nplayers (ncards-1) (snd newDeck) where
    newDeck = dealCardsPlayer deck nplayers dict

dealCardsPlayer :: Deck -> Int -> HiddenDict PlayerID [Card] -> (Deck, HiddenDict PlayerID [Card])
dealCardsPlayer deck (-1) dict = (deck, dict)
dealCardsPlayer deck n dict =
    if isNothing currentHand then
        dealCardsPlayer (first3 newDeck) (n-1) (insert n (second3 newDeck : emptyHand) dict)
    else
        dealCardsPlayer (first3 newDeck) (n-1) (update n (\ ch -> second3 newDeck : fromJust ch) dict)
    where
        currentHand = get n dict
        newDeck = drawTopCard deck []

getHand :: HiddenDict PlayerID [Card] -> Int -> [Card]
getHand dict currplay
    | isJust hand = fromJust hand
    | otherwise = []
    where
        hand = get currplay dict

-- NOTE: need to watch out for case where discard pile is empty
-- deck to draw from, discard pile
drawTopCard :: Deck -> Deck -> (Deck, Card, Deck)
drawTopCard [] discard = (d, c, emptyDeck) where
    (c:d) = shuffle discard 15632
drawTopCard (c:d) discard = (d, c, discard)

drawCards :: Deck -> Deck -> Int -> (Deck, [Card], Deck)
drawCards deck discard 0 = (deck, [], discard)
drawCards deck discard n = (finalDeck, card:hand, finalDiscard) where
    (newDeck, card, newDiscard) = drawTopCard deck discard
    (finalDeck, hand, finalDiscard) = drawCards newDeck newDiscard (n-1)

-- TODO: implement randomizer
determineStartPlayer :: Int -> Int -> Int
determineStartPlayer numplayers seed = seed `mod` numplayers

-- TODO: implement randomizer
shuffle :: [Card] -> Int -> [Card]
shuffle [] _ = []
shuffle deck seed = deck !! pos:shuffle (fst (splitDeck pos deck) ++ snd (splitDeck pos deck)) seed where
    pos = (seed `mod` length deck)

splitDeck :: Int -> [Card] -> ([Card],[Card])
splitDeck _ [] = ([],[])
splitDeck pos deck
    | pos == 0 = ([],tail deck)
    | pos == length deck - 1 = (fst (splitAt pos deck),[])
    | otherwise =  (fst (splitAt pos deck),tail (snd (splitAt pos deck)))

-- SETUP STEPS:
    -- create starting deck
    -- deal cards to all players
        -- these form the hand dictionary
    -- determine starting player + order
    -- reveal top card
initWorld :: Int -> Int -> State
initWorld nplayers ncards = State (Aura 0 0 0) (first3 initData) (second3 initData) emptyDeck (third3 initData) nplayers (determineStartPlayer nplayers 72684) 1 where
    initData = dealCards (shuffle startingDeck 8675309) nplayers ncards emptyDict

-- handHasPlayable (Aura 0 0 0) (Card 1 3) [] => False
-- handHasPlayable (Aura 0 0 0) (Card 1 3) [(Card 4 9), (Card 0 13)] => True
-- handHasPlayable (Aura 0 0 0) (Card 1 5) [(Card 2 4), (Card 3 12), (Card 4 2)] => False
-- handHasPlayable (Aura 0 0 0) (Card 1 5) [(Card 2 4), (Card 3 5), (Card 4 2)] => True
-- handHasPlayable (Aura 1 0 2) (Card 0 13) [(Card 4 2), (Card 2 5), (Card 1 11)] => True
-- handHasPlayable (Aura 2 4 0) (Card 4 12) [(Card 2 4), (Card 3 5), (Card 4 2)] => False
-- handHasPlayable (Aura 2 4 0) (Card 4 12) [(Card 3 5), (Card 2 12), (Card 0 14)] => True
-- handHasPlayable (Aura 4 4 0) (Card 4 12) [(Card 3 12), (Card 0 14), (Card 1 10)] => True
handHasPlayable :: Aura -> Card -> [Card] -> Bool
handHasPlayable _ _ [] = False
handHasPlayable aura card hand = isCardPlayable aura card (head hand) || handHasPlayable aura card (tail hand)

-- getHandPlayable (Aura 0 0 0) (Card 1 3) []
--     => []
-- getHandPlayable (Aura 0 0 0) (Card 1 3) [(Card 2 4), (Card 1 1), (Card 3 12), (Card 4 3), (Card 1 9), (Card 0 13), (Card 0 14)]
--     => [(Card 1 1), (Card 4 3), (Card 1 9), (Card 0 13), (Card 0 14)]
-- getHandPlayable (Aura 0 0 0) (Card 4 11) [(Card 2 4), (Card 1 1), (Card 3 12), (Card 3 3), (Card 1 9)]
--     => []
-- getHandPlayable (Aura 1 0 3) (Card 0 13) [(Card 2 4), (Card 1 1), (Card 3 12), (Card 4 3), (Card 1 9), (Card 0 13), (Card 0 14)]
--     => [(Card 3 12), (Card 0 13), (Card 0 14)]
-- getHandPlayable (Aura 2 6 0) (Card 0 13) [(Card 2 4), (Card 1 1), (Card 3 12), (Card 4 3), (Card 1 9), (Card 0 13), (Card 0 14)]
--     => [(Card 3 12), (Card 0 14)]
-- getHandPlayable (Aura 4 4 1) (Card 0 13) [(Card 2 4), (Card 1 1), (Card 3 12), (Card 4 3), (Card 1 9), (Card 0 13), (Card 0 14)]
--     => [(Card 0 14)]
getHandPlayable :: Aura -> Card -> [Card] -> [Card]
getHandPlayable aura card = filter (isCardPlayable aura card)
{-getHandPlayable aura card hand
    | isCardPlayable aura card (head hand) = head hand:getHandPlayable aura card (tail hand)
    | otherwise = getHandPlayable aura card (tail hand)-}

-- isCardPlayable (Aura 0 0 0) (Card 1 3) (Card 4 9) => False
-- isCardPlayable (Aura 0 0 0) (Card 1 3) (Card 1 10) => True
-- isCardPlayable (Aura 0 0 0) (Card 1 3) (Card 3 3) => True
-- isCardPlayable (Aura 0 0 0) (Card 1 3) (Card 0 13) => True
-- isCardPlayable (Aura 1 0 2) (Card 0 13) (Card 4 2) => False
-- isCardPlayable (Aura 1 0 2) (Card 0 13) (Card 2 5) => True
-- isCardPlayable (Aura 1 0 2) (Card 0 13) (Card 0 14) => True
-- isCardPlayable (Aura 2 4 0) (Card 4 12) (Card 2 4) => False
-- isCardPlayable (Aura 2 4 0) (Card 4 12) (Card 0 13) => False
-- isCardPlayable (Aura 2 4 0) (Card 4 12) (Card 2 12) => True
-- isCardPlayable (Aura 2 4 0) (Card 4 12) (Card 0 14) => True
-- isCardPlayable (Aura 4 4 3) (Card 0 14) (Card 1 3) => False
-- isCardPlayable (Aura 4 4 3) (Card 0 14) (Card 3 12) => False
-- isCardPlayable (Aura 4 4 3) (Card 0 14) (Card 0 13) => False
-- isCardPlayable (Aura 4 4 3) (Card 0 14) (Card 0 14) => True
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

-- removeCardFromHand (Card 2 0) []
--     => []
-- removeCardFromHand (Card 1 8) [(Card 2 11), (Card 0 13), (Card 1 1)]
--     => [(Card 2 11), (Card 0 13), (Card 1 1)]
-- removeCardFromHand (Card 3 4) [(Card 1 5), (Card 4 4), (Card 0 14), (Card 3 4), (Card 2 2), (Card 3 4)]
--     => [(Card 1 5), (Card 4 4), (Card 0 14), (Card 2 2), (Card 3 4)]
removeCardFromHand :: Card -> [Card] -> [Card]
removeCardFromHand _ [] = []
removeCardFromHand card hand
    | card == head hand = tail hand
    | otherwise = head hand : removeCardFromHand card (tail hand)

endRound :: State -> Action -> State
endRound (State aura deck (Card col num) discard dict nplay currplay dir) action = State (nextAura (Card col num) action aura) deck (Card col num) discard dict nplay (fst dirplay) (snd dirplay) where
    dirplay = if num == 11 then ((currplay + (*) (-1) dir) `mod` nplay,(*) (-1) dir) else ((currplay + dir) `mod` nplay, dir)

nextAura :: Card -> Action -> Aura -> Aura
nextAura (Card tcol tnum) Draw (Aura state num col)
    | state `elem` [1,4] = Aura 1 0 col
    | otherwise = Aura 0 0 0
nextAura (Card tcol tnum) (Play _ nextcol _) (Aura state num col)
    | tnum == 14 = Aura 4 (num + 4) nextcol
    | tnum == 13 = Aura 1 0 nextcol
    | tnum == 12 = Aura 2 (num + 2) col
    | tnum == 10 = Aura 3 0 0
    | otherwise = Aura 0 0 0

-- draw deck, top discard, other discards, dictionary of player hands
-- number of players, current player, play direction
-- data State = State Deck Card [Card] (HiddenDict PlayerID [Card]) Int PlayerID Int

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
