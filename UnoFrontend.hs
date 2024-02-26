-- UI for the uno game
module UnoFrontend where

import UnoBackend
import HiddenDict
import UnoPlayers
import System.CPUTime
import Data.Maybe (isNothing, isJust, fromJust)
import Text.Read (readMaybe)
import System.Exit
import Text.Printf
import Control.Exception
import Control.Monad
import Data.Char (toUpper)
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
readCard (h0:t0)
    | h `elem` ['R','Y','G','B'] = if typeNum >= 0 && typeNum <= 12
        then (Just (Card colNum typeNum), True)
        else (Nothing, False)
    | h == 'W' = if wildNum `elem` [13, 14]
        then (Just (Card 0 wildNum), True)
        else (Nothing, False)
    | h == '+' && t == "4" = (Just (Card 0 14), True)
    | otherwise = (Nothing, False)
    where
        h = toUpper h0
        t = map toUpper t0
        tInt = readMaybe t0 :: Maybe Int
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
    putStrLn "What would you like to do?"
    putStrLn "   1 - Play UNO!"
    putStrLn "   2 - Edit Config (TODO)"
    putStrLn "   0 - Quit"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> do
            putStrLn "\nAlright, let's get this game started!\n"
            return (Just (initWorld 4 7))
        Just 2 -> do
            -- TODO: add config menu
            putStrLn "\nThis hasn't been implemented yet! Please choose a different option.\n"
            setup
        Just 0 -> exitSuccess
        _ -> do
            putStrLn "\nInvalid input.\n"
            setup
{- 
config :: Rules -> IO Rules
config (Rules two four seven zero draw decks) = do
    putStrLn "Welcome to rules select! Select which rule you would like to toggle."
    putStrLn "   1 - Players can stack +2 cards on other +2 cards: " ++ two
    putStrLn "   2 - +4 cards can stack on +2 cards and +4 cards: " ++ four
    putStrLn "   3 - Playing a 7 allows you to swap hands with an opponent: " ++ seven
    putStrLn "   4 - Playing a zero rotates all hands in the direction of play: " ++ zero
    putStrLn "   5 - When drawing cards, keep drawing until you draw a playable card: " ++ draw
    putStrLn "   6 - Starting number of decks: " ++ decks
    putStrLn "   0 - Save and exit to menu"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line of
        Just 1 -> do
            putStrLn "\nConfig 1 changed.\n"
            config (Rules (not two) four seven zero draw decks)
        Just 2 -> do
            putStrLn "\nConfig 2 changed.\n"
            config (Rules two (not four) seven zero draw decks)
        Just 3 -> do
            putStrLn "\nConfig 3 changed.\n"
            config (Rules two four (not seven) zero draw decks)
        Just 4 -> do
            putStrLn "\nConfig 4 changed.\n"
            config (Rules two four seven (not zero) draw decks)
        Just 5 -> do
            putStrLn "\nConfig 5 changed.\n"
            config (Rules two four seven zero (not draw) decks)
        Just 6 -> do
            putStrLn "\nHow many decks?\n"
            line2 <- readLine
            -- TODO: implement guards on number of decks (1-10 max maybe)
            if (isJust (readMaybe line2 :: Maybe Int)) then config (Rules two four seven zero draw line2)
            "\nInvalid input.\n"
            config (Rules two four seven zero draw decks)
        Just 0 -> return (Rules two four seven zero draw decks)
        _ -> do
            putStrLn "\nInvalid input.\n"
            config (Rules two four seven zero draw decks)

-}

main :: IO ()
main = do
    putStrLn "\nWelcome to UNO!\n"
    state <- setup
    case state of
        Just state -> do
            announceStartState state
            gameLoop state
            return ()
        Nothing -> do
            putStrLn "Could not start game. Please try again."
            return ()

announceStartState :: State -> IO ()
announceStartState (State aura deck tcard discard dict nplay currplay dir) = do
    putStrLn ("This is a " ++ show nplay ++ " player game of UNO!")
    putStrLn ("Player " ++ show currplay ++ " will go first")
    putStrLn ("The starting card is " ++ show tcard)

chooseColor :: IO Int
chooseColor = do
    putStrLn "\nYou played a wild! Choose a colour:"
    putStrLn "   1 - Red"
    putStrLn "   2 - Yellow"
    putStrLn "   3 - Green"
    putStrLn "   4 - Blue"
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

humanPlayerPlay :: Aura -> Card -> [Card] -> Int -> IO Action
humanPlayerPlay aura topcard hand dir = do
    putStr "Your hand: "
    putStrLn (show hand)
    putStr ("Top card is " ++ show topcard ++ show aura ++ ", play direction is ")
    putStrLn ((\ d -> if d == 1 then "clockwise" else "counterclockwise") dir)
    
    if handHasPlayable aura topcard hand then do
        putStrLn "\nWhat would you like to do?"
        putStrLn "   - Play: type the name of the card as shown above, in quotation marks"
        putStrLn "   - Draw: type \"draw\", in quotation marks"
        putStrLn "   - Quit: type CTRL+C"
    else do
        putStrLn "\nYou have no playable cards, so you must draw"
        putStrLn "   - Draw: type \"draw\", in quotation marks"
        putStrLn "   - Quit: type CTRL+C"
    putStr "Your choice: "
    line <- getLine
    case readMaybe line :: Maybe [Char] of
        Just "draw" -> do
            return Draw
        Just t -> do
            if isJust cardToPlay && Card jCol jNum `elem` hand && isCardPlayable aura topcard (Card jCol jNum) then do
                if jCol == 0 then do
                    nextcol <- chooseColor
                    return (Play (Card jCol jNum) nextcol unoCalled)
                else do
                    return (Play (Card jCol jNum) 0 unoCalled)
            else do
                putStrLn "\nInvalid input.\n"
                humanPlayerPlay aura topcard hand dir
            where
                (cardToPlay, unoCalled) = readCard t
                (Card jCol jNum) = fromJust cardToPlay
        _ -> do
            putStrLn "\nInvalid input.\n"
            humanPlayerPlay aura topcard hand dir

humanDrawFunc :: [Card] -> IO ()
humanDrawFunc cards = do
    putStrLn ("You drew: " ++ show cards)

botDrawFunc :: [Card] -> IO ()
botDrawFunc cards = do
    return ()

handlePlaySequence :: State -> Action -> [Card] -> ([Card] -> IO ()) -> IO (Maybe State)
handlePlaySequence (State (Aura state num col) deck tcard discard dict nplay currplay dir) action hand drawFunc = case action :: Action of
    Draw -> do
        if state `elem` [2,4] then do
            let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck discard currplay num dict
            putStrLn ("Player " ++ show currplay ++ " draws " ++ show num ++ " cards")
            drawFunc drawnCards
            return (Just (endRound (State (Aura state num col) newDeck tcard newDiscard newDict nplay currplay dir) action))
        else do
            let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck discard currplay 1 dict
            putStrLn ("Player " ++ show currplay ++ " draws a card")
            drawFunc drawnCards
            return (Just (endRound (State (Aura state num col) newDeck tcard newDiscard newDict nplay currplay dir) action))
    (Play card col unoCalled) -> do
        putStrLn ("Player " ++ show currplay ++ " plays " ++ show card)
        if null newHand then do
            -- OUT OF CARDS
            putStrLn ("Player " ++ show currplay ++ " has played the last card in their hand and wins the game!")
            putStrLn "\n--- GAME OVER ---"
            return Nothing
        else if length newHand == 1 then do
            -- UNO CASE
            announceCardPlayed (Aura state num col) card col
            if unoCalled then do
                putStrLn ("Player " ++ show currplay ++ " has called UNO!")
                return (Just (endRound (State (Aura state num col) deck card (tcard:discard) (insert currplay newHand dict) nplay currplay dir) action))
            else do
                putStrLn ("Player " ++ show currplay ++ " forgot to call UNO! and must draw 4 cards")
                let (newDeck, newDiscard, drawnCards, newDict) = playerIsAskedToDrawCards deck (tcard:discard) currplay 4 (insert currplay newHand dict)
                drawFunc drawnCards
                return (Just (endRound (State (Aura state num col) newDeck card newDiscard newDict nplay currplay dir) action))
        else do
            -- GENERAL CASE
            announceCardPlayed (Aura state num col) card col
            return (Just (endRound (State (Aura state num col) deck card (tcard:discard) (insert currplay newHand dict) nplay currplay dir) action))
        where
            newHand = removeCardFromHand card hand

gameLoop :: State -> IO ()
gameLoop (State (Aura state num col) deck tcard discard dict nplay currplay dir) = do
    putStrLn ("\nIt is player " ++ show currplay ++ "\'s turn")
    case currplay :: Int of
        0 -> do
            if state == 3 then do
                putStrLn ("Oh no! Player " ++ show currplay ++ "\'s turn has been skipped!")
                gameLoop (endRound (State (Aura state num col) deck tcard discard dict nplay currplay dir) Draw)
            else do
                putStrLn "Your turn!\n"
                action <- humanPlayerPlay (Aura state num col) tcard hand dir
                newState <- handlePlaySequence (State (Aura state num col) deck tcard discard dict nplay currplay dir) action hand humanDrawFunc
                if isJust newState then do
                    gameLoop (fromJust newState)
                else do
                    main
                where
                    hand = getHand dict currplay
        _ -> do
            if state == 3 then do
                putStrLn ("Oh no! Player " ++ show currplay ++ "\'s turn has been skipped!")
                gameLoop (endRound (State (Aura state num col) deck tcard discard dict nplay currplay dir) Draw)
            else do
                let action = firstCardPlay (Aura state num col) tcard hand dir
                newState <- handlePlaySequence (State (Aura state num col) deck tcard discard dict nplay currplay dir) action hand botDrawFunc
                if isJust newState then do
                    gameLoop (fromJust newState)
                else do
                    main
                where
                    hand = getHand dict currplay

-- nice code lol
-- this is just a copy of the handlePlaySequence code haha - it was originally in gameLoop

announceCardPlayed :: Aura -> Card -> Int -> IO ()
announceCardPlayed (Aura state n c) (Card col num) nextcol = do
    when (col == 0) $ do
        putStrLn ("They have chosen the colour " ++ stringifyCol nextcol)
    when (num `elem` [12, 14]) $ do
        putStrLn ("The next player will need to draw " ++ show (n + num - 10) ++ " cards")
    when (num == 10) $ do
        putStrLn "The next player's turn will be skipped!"
    when (num == 11) $ do
        putStrLn "The direction of play has been reversed!"

-- draw pile, discard pile, player ID, number of cards to draw, hand dict
playerIsAskedToDrawCards :: Deck -> Deck -> Int -> Int -> HiddenDict PlayerID [Card] -> (Deck, Deck, [Card], HiddenDict PlayerID [Card])
playerIsAskedToDrawCards drawpile discardpile currplay n dict =
    (newDrawPile, newDiscardPile, drawnCards, update currplay (\ ch -> drawnCards ++ fromJust ch) dict)
    where
        currentHand = getHand dict currplay
        (newDrawPile, drawnCards, newDiscardPile) = drawCards drawpile discardpile n
