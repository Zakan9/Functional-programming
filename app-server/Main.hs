{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Web.Scotty
import Control.Concurrent.STM (TVar, newTVarIO, atomically, readTVar, writeTVar)
import Control.Concurrent (Chan, newChan, forkIO, threadDelay)
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Text.Lazy qualified as TL
import System.Exit (exitFailure)
import Data.Text.Lazy.Encoding qualified as TLE

import Lib1 qualified
import Lib2 qualified
import Lib3 qualified
import Lib4 qualified

-- ============================================================================
-- Server State Management
-- ============================================================================

-- | Execute a command and return result as string
executeAndRespond :: TVar Lib3.State -> String -> IO String
executeAndRespond tvar cmdStr = do
  case Lib3.runParser Lib3.parseCommand cmdStr of
    Left err -> return $ "PARSE ERROR: " ++ err
    Right (cmd, remaining) -> 
      if null remaining || all (== ' ') remaining
      then do
        oldState <- atomically $ readTVar tvar
        let (result, newState) = executeCommandWithResult cmd oldState
        atomically $ writeTVar tvar newState
        return result
      else return $ "PARSE ERROR: remaining input: " ++ remaining

-- | Execute command and return string result (shared logic with Lib4)
executeCommandWithResult :: Lib1.Command -> Lib3.State -> (String, Lib3.State)
executeCommandWithResult cmd state = 
  let (result, newState) = executeCommandPure' cmd state
  in (formatResult result, newState)

-- | Pure command execution matching Lib3.State structure
executeCommandPure' :: Lib1.Command -> Lib3.State -> (Lib4.ExecutionResult, Lib3.State)
executeCommandPure' (Lib1.AddCar car) state =
  let newState = state { Lib3.stateCars = Lib3.stateCars state ++ [car] }
  in (Lib4.ResultOk $ "Added car: " ++ Lib1.carNumber car, newState)

executeCommandPure' (Lib1.FilterByNumber num) state =
  let result = filter (\c -> Lib1.carNumber c == num) (Lib3.stateCars state)
  in case result of
    (car:_) -> (Lib4.ResultCar (Just car), state)
    [] -> (Lib4.ResultCar Nothing, state)

executeCommandPure' (Lib1.AddMechanic carNum name age) state =
  let newCars = map (\car -> 
          if Lib1.carNumber car == carNum
          then car { Lib1.mechanics = Lib1.mechanics car ++ [Lib1.Mechanic name age] }
          else car
        ) (Lib3.stateCars state)
  in (Lib4.ResultOk $ "Added mechanic " ++ name ++ " to car " ++ carNum, state { Lib3.stateCars = newCars })

executeCommandPure' (Lib1.CalculateAverageLapTime car) state =
  let times = Lib1.lapTimesS car
  in if null times
     then (Lib4.ResultAverage Nothing, state)
     else let avg = fromIntegral (sum times) / fromIntegral (length times) :: Double
          in (Lib4.ResultAverage (Just avg), state)

executeCommandPure' (Lib1.Sequence cmd1 cmd2) state =
  let (_, state1) = executeCommandPure' cmd1 state
  in executeCommandPure' cmd2 state1

executeCommandPure' (Lib1.Dump Lib1.Examples) state =
  (Lib4.ResultExamples $ map Lib2.toCliCommand Lib1.examples, state)

-- | Format ExecutionResult as string for HTTP response
formatResult :: Lib4.ExecutionResult -> String
formatResult (Lib4.ResultOk msg) = msg
formatResult (Lib4.ResultCar (Just car)) = "Found car: " ++ show car
formatResult (Lib4.ResultCar Nothing) = "Car not found"
formatResult (Lib4.ResultAverage (Just avg)) = "Average lap time: " ++ show avg ++ "s"
formatResult (Lib4.ResultAverage Nothing) = "No lap times available"
formatResult (Lib4.ResultExamples examples) = unlines ("Examples:" : examples)

-- ============================================================================
-- Main Server Entry Point
-- ============================================================================

failOnError :: IO (Either String a) -> IO a
failOnError action = do
  result <- action
  case result of
    Right a -> return a
    Left m -> putStrLn ("Fatal error: " ++ m) >> exitFailure

main :: IO ()
main = do
  -- Initialize state
  state <- newTVarIO Lib3.emptyState
  chan <- newChan
  
  -- Start storage operation loop (for file persistence)
  _ <- forkIO $ Lib3.storageOpLoop chan
  
  -- Load state from file on startup
  _ <- failOnError $ Lib3.load chan state
  putStrLn "State loaded from file"
  
  -- Start periodic save (every 1 second)
  _ <- forkIO $ forever $ do
    threadDelay 1000000  -- 1 second
    _ <- Lib3.save chan state
    return ()
  putStrLn "Periodic save started"
  
  -- Start HTTP server on port 3000
  putStrLn "Starting server on port 3000..."
  scotty 3000 $ do
    -- GET welcome page
    get "/" $ do
      html $ TL.concat
        [ "<html><head><title>FP2025 Racing Car Server</title></head><body>"
        , "<h1>🏎️ F1 Racing Car Management Server</h1>"
        , "<h2>Available Endpoints:</h2>"
        , "<ul>"
        , "<li><b>POST /</b> - Execute a command (send command as body)</li>"
        , "<li><b>GET /state</b> - View current state</li>"
        , "<li><b>GET /health</b> - Health check</li>"
        , "</ul>"
        , "<h2>Example Commands:</h2>"
        , "<pre>"
        , "dump_examples\n"
        , "add_car Car \"44\" (Driver \"Lewis Hamilton\" \"UK\" 38) [] [63, 64]\n"
        , "filter_by_number \"44\"\n"
        , "add_mechanic \"44\" \"James\" 35\n"
        , "</pre>"
        , "<p><a href=\"/state\">View Current State</a></p>"
        , "</body></html>"
        ]
    
    -- POST endpoint for commands
    post "/" $ do
      reqBody <- body
      let cmdStr = TL.unpack $ TLE.decodeUtf8 reqBody
      result <- liftIO $ executeAndRespond state cmdStr
      text $ TL.pack result
    
    -- GET endpoint for health check
    get "/health" $ do
      text "OK"
    
    -- GET endpoint for current state
    get "/state" $ do
      currentState <- liftIO $ atomically $ readTVar state
      text $ TL.pack $ show currentState

