-- UI for the uno game
module UnoFrontend where

import UnoBackend
import HiddenDict
import System.CPUTime
import Data.Maybe (isNothing, isJust, fromJust)
import Text.Read (readMaybe)
import System.Exit
import Text.Printf
import Control.Exception
import Control.Monad
--import Data.Time

-- readCard "" => Nothing
-- readCard "3" => Nothing
-- readCard "R0" => Just R0
-- readCard "B3" => Just B3
-- readCard "G11" => Nothing
-- readCard "+2" => Nothing
-- readCard "G+2" => Just G+2
-- readCard "Y+" => Just Y+2
-- readCard "RS" => Just R<X>
-- readCard "W" => Just WILD
-- readCard "W4" => Just W+4
-- readCard "+4" => Just W+4
-- readCard "W3" => Nothing
readCard :: String -> (Maybe Card, Bool)
readCard (h:t)
    | h `elem` ['R','Y','G','B'] = if typeNum >= 0 && typeNum <= 12
        then (Just (Card colNum typeNum), False)
        else (Nothing, False)
    | h == 'W' = if wildNum `elem` [13, 14]
        then (Just (Card 0 wildNum), False)
        else (Nothing, False)
    | h == '+' && t == "4" = (Just (Card 0 14), False)
    | otherwise = (Nothing, False) where
        tInt = readMaybe t :: Maybe Int
        colNum
            | h == 'R' = 1
            | h == 'Y' = 2
            | h == 'G' = 3
            | otherwise = 4
        typeNum
            | t `elem` ["<X>", "X", "S"] = 10
            | t `elem` ["<=>", "=", "R"] = 11
            | t `elem` ["+2", "+"] = 12
            | isJust tInt && fromJust tInt >= 0 && fromJust tInt <= 9 = fromJust tInt
            | otherwise = 14
        wildNum
            | t `elem` ["ILD", "I", "L", "D", ""] = 13
            | t `elem` ["+4", "+", "4"] = 14
            | otherwise = 0
readCard _ = (Nothing, False)

setup :: IO (Maybe State)
setup = do
    putStrLn "What would you like to do?\n"
    putStrLn "\t 1. Play Uno!"
    putStrLn "\t 2. Edit Config (TODO)"
    putStrLn "\t 9. Quit"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> do
            return (Just (initWorld 4 7))
        Just 9 -> exitSuccess
        _ -> do
            putStrLn "\nInvalid input.\n"
            setup

test = main

main :: IO ()
main = do
    putStrLn "\nWelcome to Uno!\n"
    state <- setup
    case state of
        Just state -> do
            gameLoop state
            return ()
        Nothing -> do
            putStrLn "Could not start game. Please try again later."
            return ()

chooseColor :: IO Int
chooseColor = do
    putStrLn "\nYou played a wild! Choose a colour:"
    putStrLn "\t 1. Red"
    putStrLn "\t 2. Yellow"
    putStrLn "\t 3. Green"
    putStrLn "\t 4. Blue"
    putStr "\nYour choice: "
    line <- getLine
    case readMaybe line :: Maybe Int of
        Just 1 -> return 1
        Just 2 -> return 2
        Just 3 -> return 3
        Just 4 -> return 4
        _ -> do
            putStrLn "\nInvalid input."
            chooseColor

-- https://wiki.haskell.org/Timing_computations
timedUnoCall :: IO ()
timedUnoCall = do

    putStrLn "\nQuick say Uno!!"
    start <- getCPUTime
    line <- getLine
    putStrLn line

    case readMaybe line :: Maybe Int of
        Just 1 -> do
            end   <- getCPUTime
            let diff = fromIntegral (end - start) / (10^12)
            putStrLn (show diff)
            if diff < 2.00
                then do
                    putStrLn "\nOh no your enemy called you out faster!!"
                    -- TODO DRAW TWO
                else do
                     putStrLn "\nGreat Job King!!"
        Just _ -> do
            putStrLn "\nRecognized"
        _ -> do
             putStrLn "\nTyping can be hard sometimes, I get it, try again"

humanPlayerPlay :: Aura -> Card -> Hand -> IO Action
humanPlayerPlay aura topcard hand = do
    putStr "\nYour hand: "
    putStr (show hand)
    if handHasPlayable aura topcard hand then do
        putStrLn "\nWhat would you like to do?"
        putStrLn "\t - Play: type the name of the card as shown above"
        putStrLn "\t - Draw: type \"draw\""
    else do
        putStrLn "\nYou have no playable cards, so you must draw"
        putStrLn "\t - Draw: type \"draw\""

    putStr "Your choice: "
    line <- getLine
    case readMaybe line :: Maybe [Char] of
        Just "draw" -> do
            return Draw
        Just t -> do
            putStrLn "Parsing card..."
            if isJust cardToPlay && Card jCol jNum `elem` hand && isCardPlayable aura topcard (Card jCol jNum) then do
                if jCol == 0 then do
                    nextcol <- chooseColor
                    return (Play (Card jCol jNum) nextcol unoCalled)
                else do
                    return (Play (Card jCol jNum) 0 unoCalled)
            else do
                putStrLn "\nInvalid input.\n"
                humanPlayerPlay aura topcard hand
            where
                (cardToPlay, unoCalled) = readCard line
                (Card jCol jNum) = fromJust cardToPlay
        _ -> do
            putStrLn "\nInvalid input.\n"
            humanPlayerPlay aura topcard hand

gameLoop :: State -> IO ()
gameLoop (State (Aura state num col) deck tcard discard dict nplay currplay dir) = do
    putStrLn ("It is player " ++ show currplay ++ "'s turn \n")
    case currplay :: Int of
        0 -> do
            putStrLn "It's up to you!\n"
            action <- humanPlayerPlay (Aura state num col) tcard hand
            case action :: Action of
                Draw -> do
                    if state `elem` [2,4] then do
                        let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck discard currplay num dict
                        putStrLn ("Player " ++ show currplay ++ " draws " ++ show num ++ " cards")
                        putStrLn ("You drew: " ++ show drawnCards)
                        gameLoop (endRound (State (Aura state num col) newDeck tcard newDiscard newDict nplay currplay dir) action)
                    else do
                        let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck discard currplay 1 dict
                        putStrLn ("Player " ++ show currplay ++ " draws a card")
                        putStrLn ("You drew: " ++ show (head drawnCards))
                        gameLoop (endRound (State (Aura state num col) newDeck tcard newDiscard newDict nplay currplay dir) action)
                (Play card col unoCalled) -> do
                    putStrLn ("Player " ++ show currplay ++ " plays " ++ show card)
                    if null newHand then do
                        -- OUT OF CARDS
                        putStrLn ("Player " ++ show currplay ++ " has played the last card in their hand and wins the game!")
                        putStrLn "--- GAME OVER ---"
                        main
                    else if length newHand == 1 then do
                        -- UNO CASE
                        announceCardPlayed (Aura state num col) card col
                        if unoCalled then do
                            putStrLn ("Player " ++ show currplay ++ " has called UNO!")
                            gameLoop (endRound (State (Aura state num col) deck card (tcard:discard) (insert currplay newHand dict) nplay currplay dir) action)
                        else do
                            putStrLn ("Player " ++ show currplay ++ " forgot to call UNO! and must draw 4 cards")
                            let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck (tcard:discard) currplay 4 (insert currplay newHand dict)
                            putStrLn ("You drew: " ++ show drawnCards)
                            gameLoop (endRound (State (Aura state num col) newDeck card newDiscard newDict nplay currplay dir) action)
                    else do
                        -- GENERAL CASE
                        announceCardPlayed (Aura state num col) card col
                        gameLoop (endRound (State (Aura state num col) deck card (tcard:discard) (insert currplay newHand dict) nplay currplay dir) action)
                    where
                        newHand = removeCardFromHand card hand
            where
                hand = getHand dict currplay
        _ -> do
            -- TODO BOT TURN
            putStrLn "A bot player is thinking…\n"
            --return (State aura deck tcard discard dict nplay currplay dir)
            gameLoop (endRound (State (Aura state num col) deck tcard discard dict nplay currplay dir) Draw)

announceCardPlayed :: Aura -> Card -> Int -> IO ()
announceCardPlayed (Aura state n c) (Card col num) nextcol = do
    when (col == 0) $ do
        putStrLn ("The chosen colour is " ++ stringifyCol nextcol)
    when (num `elem` [12, 14]) $ do
        putStrLn ("The next player will need to draw " ++ show (n + num - 10) ++ " cards")

-- draw pile, discard pile, player ID, number of cards to draw, hand dict
playerIsAskedToDrawCards :: Deck -> Deck -> Int -> Int -> HiddenDict PlayerID Hand -> (Deck, Deck, [Card], HiddenDict PlayerID Hand)
playerIsAskedToDrawCards drawpile discardpile currplay n dict =
    (newDrawPile, newDiscardPile, drawnCards, update currplay (\ ch -> drawnCards ++ fromJust ch) dict)
    where
        currentHand = getHand dict currplay
        (newDrawPile, drawnCards, newDiscardPile) = drawCards drawpile discardpile n