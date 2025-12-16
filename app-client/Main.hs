{-# LANGUAGE ImportQualifiedPost #-}
module Main (main) where

import Lib1 qualified
import Lib3 qualified
import Lib4 qualified
import Control.Monad.Trans.State.Strict (runState)

-- ============================================================================
-- Example Programs using Free Monad DSL
-- ============================================================================

-- | Example program that adds cars and queries them
exampleProgram :: Lib4.CommandProgram [Lib4.ExecutionResult]
exampleProgram = do
  -- Create a sample car
  let car1 = Lib1.Car 
        { Lib1.carNumber = "44"
        , Lib1.driver = Lib1.Driver "Lewis Hamilton" "UK" 38
        , Lib1.mechanics = []
        , Lib1.lapTimesS = [63, 64, 62]
        }
  
  let car2 = Lib1.Car 
        { Lib1.carNumber = "1"
        , Lib1.driver = Lib1.Driver "Max Verstappen" "Netherlands" 26
        , Lib1.mechanics = [Lib1.Mechanic "John" 40]
        , Lib1.lapTimesS = [60, 58, 59]
        }
  
  -- Add cars
  r1 <- Lib4.addCar car1
  r2 <- Lib4.addCar car2
  
  -- Add a mechanic to car 44
  r3 <- Lib4.addMechanic "44" "Sophia" 27
  
  -- Filter by number
  r4 <- Lib4.filterByNumber "44"
  
  -- Calculate average lap time
  r5 <- Lib4.calculateAverageLapTime car1
  
  -- Dump examples
  r6 <- Lib4.dumpExamples
  
  return [r1, r2, r3, r4, r5, r6]

-- | Simpler program for demo
simpleProgram :: Lib4.CommandProgram Lib4.ExecutionResult
simpleProgram = do
  let car = Lib1.Car 
        { Lib1.carNumber = "16"
        , Lib1.driver = Lib1.Driver "Charles Leclerc" "Monaco" 25
        , Lib1.mechanics = []
        , Lib1.lapTimesS = [59, 59, 61]
        }
  _ <- Lib4.addCar car
  Lib4.filterByNumber "16"

-- ============================================================================
-- Main Entry Point
-- ============================================================================

main :: IO ()
main = do
  putStrLn "========================================="
  putStrLn "   Free Monad DSL Client Demo"
  putStrLn "========================================="
  putStrLn ""
  
  -- ----------------------------------------
  -- Demo 1: Local State Interpreter
  -- ----------------------------------------
  putStrLn "--- Local State Interpreter Demo ---"
  putStrLn ""
  
  let (results, finalState) = Lib4.runLocal exampleProgram
  
  putStrLn "Results from local interpreter:"
  mapM_ (putStrLn . ("  " ++) . formatResult) results
  putStrLn ""
  
  putStrLn $ "Final state has " ++ show (length $ Lib4.stateCars finalState) ++ " cars:"
  mapM_ (\car -> putStrLn $ "  - Car #" ++ Lib1.carNumber car ++ " (" ++ Lib1.driverName (Lib1.driver car) ++ ")") 
        (Lib4.stateCars finalState)
  putStrLn ""
  
  -- ----------------------------------------
  -- Demo 2: Simple program
  -- ----------------------------------------
  putStrLn "--- Simple Program Demo ---"
  putStrLn ""
  
  let (simpleResult, simpleState) = Lib4.runLocal simpleProgram
  putStrLn $ "Simple program result: " ++ formatResult simpleResult
  putStrLn $ "State after simple program: " ++ show (length $ Lib4.stateCars simpleState) ++ " cars"
  putStrLn ""
  
  -- ----------------------------------------
  -- Demo 3: HTTP Interpreter (optional)
  -- ----------------------------------------
  putStrLn "--- HTTP Interpreter Demo ---"
  putStrLn ""
  putStrLn "To test the HTTP interpreter:"
  putStrLn "  1. Start the server: stack run fp2025-server"
  putStrLn "  2. The client would send commands to http://localhost:3000"
  putStrLn ""
  putStrLn "Example: Lib4.interpretHttp \"http://localhost:3000\" simpleProgram"
  putStrLn ""
  
  -- Uncomment to actually test HTTP interpreter (requires server running):
  -- putStrLn "Testing HTTP interpreter..."
  -- httpResult <- Lib4.interpretHttp "http://localhost:3000" simpleProgram
  -- putStrLn $ "HTTP result: " ++ formatResult httpResult
  
  putStrLn "========================================="
  putStrLn "   Demo Complete!"
  putStrLn "========================================="

-- | Format execution result for display
formatResult :: Lib4.ExecutionResult -> String
formatResult (Lib4.ResultOk msg) = "OK: " ++ msg
formatResult (Lib4.ResultCar (Just car)) = "Found: Car #" ++ Lib1.carNumber car
formatResult (Lib4.ResultCar Nothing) = "Not found"
formatResult (Lib4.ResultAverage (Just avg)) = "Average: " ++ show avg ++ "s"
formatResult (Lib4.ResultAverage Nothing) = "No lap times"
formatResult (Lib4.ResultExamples exs) = "Examples: " ++ show (length exs) ++ " items"
