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
readCard :: String -> Maybe Card
readCard (h:t)
    | h `elem` ['R','Y','G','B'] = if typeNum >= 0 && typeNum <= 12
        then Just (Card colNum typeNum)
        else Nothing
    | h == 'W' = if wildNum `elem` [13, 14]
        then Just (Card 0 wildNum)
        else Nothing
    | h == '+' && t == "4" = Just (Card 0 14)
    | otherwise = Nothing where
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
readCard _ = Nothing

setup :: IO (Maybe State)
setup = do
    putStrLn "Hi you ready to play some uno?\n"
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

main :: IO ()
main = do
    putStrLn "\nWelcome to Uno!\n"
    state <- setup
    case state of 
        Just state -> do
            -- TODO: play game from start state
            return ()
        Nothing ->
            return ()

chooseColor :: IO Int 
chooseColor = do
    putStrLn "\nYou played a wild! Select a colour:"
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

playerPlay :: State -> IO Action
playerPlay (State aura deck tcard discard dict nplay currplay dir) = do
    putStr "\nYour hand: "
    case currHand of
        Just hand -> do
            putStr (show hand)
            if handHasPlayable aura tcard hand then do
                putStrLn "\nWhat would you like to do?"
                putStrLn "\t - Play a card: type the name of the card as shown above"
                putStrLn "\t - Draw a card: type \"draw\""
            else do
                putStrLn "\nYou have no playable cards, so you must draw a card"
                putStrLn "\t - Draw a card: type \"draw\""
        _ -> do
            -- NOTE: this is an error case: if a player plays their last card, that should have been handled before getting here
            putStr "[empty]"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line :: Maybe [Char] of
        Just "draw" -> do
            return Draw
        Just t -> do
            if isJust cardToPlay && jCard `elem` jHand && isCardPlayable aura tcard jCard then do
                return (Play jCard)
            else do
                putStrLn "\nInvalid input.\n"
                playerPlay (State aura deck tcard discard dict nplay currplay dir)
            where
                cardToPlay = readCard line
                jCard = fromJust cardToPlay
        _ -> do
            putStrLn "\nInvalid input.\n"
            playerPlay (State aura deck tcard discard dict nplay currplay dir)
    where
        currHand = get currplay dict
        jHand = fromJust currHand
