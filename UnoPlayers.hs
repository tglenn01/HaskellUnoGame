module UnoPlayers where

import UnoBackend
import HiddenDict
import Data.Maybe (isJust, fromJust)

-- SIMPLE BOT PLAYER 1: always chooses the first possible playable card, or draws if no cards are playable
firstCardPlay :: Aura -> Card -> [Card] -> Int -> Action
firstCardPlay aura tcard hand dir
    | handHasPlayable aura tcard hand = Play (head (getHandPlayable aura tcard hand)) (maxWildColChooser hand) True
    | otherwise = Draw

-- maxWildColChooser [(Card 1 1), (Card 1 2), (Card 2 3), (Card 4 4), (Card 4 5), (Card 4 6), (Card 0 13)] => 4
-- maxWildColChooser [(Card 2 1), (Card 1 2), (Card 2 3), (Card 2 4), (Card 4 5), (Card 4 6), (Card 4 12)] => 2 or 4
-- maxWildColChooser [(Card 2 1), (Card 1 2), (Card 2 3), (Card 2 4), (Card 4 5), (Card 2 6), (Card 4 12)] => 2
-- maxWildColChooser [(Card 0 13), (Card 3 1), (Card 0 14), (Card 0 13)] => 3
maxWildColChooser :: [Card] -> Int
maxWildColChooser hand = fst (foldr (\ (c0,v) (c,m) -> if v > m then (c0,v) else (c,m)) (1,0)
    [(i, length (filter (\ (Card col _) -> col == i) hand)) | i <- [1..4]])