module MyTime where

import System.CPUTime
import Text.Read (readMaybe)
import Text.Printf
import Control.Exception

import Data.Time

import Data.Time.Clock

-- COMMENT THIS OUT IF NEEDED

-- https://wiki.haskell.org/Timing_computations
timedUnoCall :: IO ()
timedUnoCall = do

    putStrLn "\nQuick say Uno!!"
    timer <- newTimer  (2 * 1000000)
    line <- getLine
    putStrLn line
    myend   <- getCPUTime

    case readMaybe line :: Maybe String of
        Just "Uno" -> do 
            end   <- getCPUTime
            let diff = fromIntegral (myend - mystart) / (10^12)
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
    
    


