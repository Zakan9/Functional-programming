{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib4
  ( parseCommand
  -- * Free Monad DSL
  , CommandDSL(..)
  , CommandProgram
  , addCar
  , filterByNumber
  , addMechanic
  , calculateAverageLapTime
  , dumpExamples
  -- * Interpreters
  , interpretHttp
  , interpretLocal
  , runLocal
  -- * State
  , LocalState(..)
  , emptyLocalState
  -- * Shared execution logic
  , ExecutionResult(..)
  , executeCommandPure
  ) where

import qualified Lib1
import qualified Lib2
import Test.QuickCheck (Arbitrary, Gen, arbitrary, elements, listOf, oneof, sized, resize)

import Control.Monad.Trans.State.Strict (State, get, put, runState)
import Control.Monad.Trans.Except (ExceptT, throwE, runExceptT)
import Control.Monad.Trans.Class (lift)
import Control.Applicative (Alternative(..))
import Control.Monad.Free (Free(..), liftF)
import Network.HTTP.Simple (httpLBS, getResponseBody, parseRequest, setRequestBodyLBS, setRequestMethod)
import qualified Data.ByteString.Lazy.Char8 as LBS

-- ============================================================================
-- Parser Type and Basic Combinators
-- ============================================================================

type ErrorMsg = String
type Input = String
type Parser = ExceptT ErrorMsg (State Input)

-- | Get current input
getInput :: Parser String
getInput = lift Control.Monad.Trans.State.Strict.get

-- | Set input
putInput :: String -> Parser ()
putInput s = lift $ Control.Monad.Trans.State.Strict.put s

-- | Parse failure
parseError :: String -> Parser a
parseError = throwE

-- ============================================================================
-- Basic Terminal Parsers
-- ============================================================================

-- BNF: <char> ::= any single character matching expected
parseChar :: Char -> Parser Char
parseChar expected = do
  input <- getInput
  case input of
    [] -> parseError $ "Expected '" ++ [expected] ++ "' but got end of input"
    (c:rest) -> 
      if c == expected 
      then putInput rest >> return c
      else parseError $ "Expected '" ++ [expected] ++ "' but got '" ++ [c] ++ "'"

-- BNF: <string> ::= sequence of expected characters
parseString :: String -> Parser String
parseString expected = do
  input <- getInput
  if take (length expected) input == expected
  then putInput (drop (length expected) input) >> return expected
  else parseError $ "Expected '" ++ expected ++ "'"

-- BNF: <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

-- BNF: <number> ::= <digit>+
parseNumber :: Parser Int
parseNumber = do
  input <- getInput
  case span isDigitChar input of
    ([], _) -> parseError "Expected number"
    (digits, rest) -> putInput rest >> return (read digits)

-- BNF: <whitespace-char> ::= " " | "\t" | "\n"
isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- BNF: <whitespace> ::= <whitespace-char>*
parseWhitespace :: Parser String
parseWhitespace = do
  input <- getInput
  let (ws, rest) = span isWhitespaceChar input
  putInput rest
  return ws

-- BNF: <whitespace1> ::= <whitespace-char>+
parseWhitespace1 :: Parser String
parseWhitespace1 = do
  input <- getInput
  case input of
    [] -> parseError "Expected whitespace"
    (c:rest) -> 
      if isWhitespaceChar c
      then do
        let (ws, rest2) = span isWhitespaceChar rest
        putInput rest2
        return (c:ws)
      else parseError "Expected whitespace"

-- BNF: <quoted-string> ::= "\"" <string-content> "\""
parseQuotedString :: Parser String
parseQuotedString = do
  input <- getInput
  case input of
    [] -> parseError "Expected quoted string"
    ('"':rest) -> 
      let (content, rest2) = span (/= '"') rest
      in case rest2 of
        ('"':rest3) -> putInput rest3 >> return content
        _ -> parseError "Unterminated quoted string"
    _ -> parseError "Expected opening quote"

-- ============================================================================
-- Domain Parsing
-- ============================================================================

-- BNF: <driver> ::= "(" "Driver" <whitespace1> <quoted-string> <whitespace1> <quoted-string> <whitespace1> <number> ")"
parseDriver :: Parser Lib1.Driver
parseDriver = 
  (\_ _ name _ nat _ age _ -> Lib1.Driver name nat age)
  <$> parseChar '('
  <*> parseString "Driver"
  <*> (parseWhitespace1 *> parseQuotedString)
  <*> parseWhitespace1
  <*> parseQuotedString
  <*> parseWhitespace1
  <*> parseNumber
  <*> parseChar ')'

-- BNF: <mechanic> ::= "(" "Mechanic" <whitespace1> <quoted-string> <whitespace1> <number> ")"
parseMechanic :: Parser Lib1.Mechanic
parseMechanic = 
  (\_ _ name _ age _ -> Lib1.Mechanic name age)
  <$> parseChar '('
  <*> parseString "Mechanic"
  <*> (parseWhitespace1 *> parseQuotedString)
  <*> parseWhitespace1
  <*> parseNumber
  <*> parseChar ')'

-- BNF: <mechanic-list-tail> ::= "" | "," <whitespace> <mechanic> <mechanic-list-tail>
parseMechanicListTail :: Parser [Lib1.Mechanic]
parseMechanicListTail = 
  parseWhitespace *> 
  ((parseChar ',' *> parseWhitespace *> ((:) <$> parseMechanic <*> parseMechanicListTail)) <|> 
   pure [])

-- BNF: <mechanic-list> ::= "[" <whitespace> "]" | "[" <whitespace> <mechanic> <mechanic-list-tail> "]"
parseMechanicList :: Parser [Lib1.Mechanic]
parseMechanicList = 
  parseChar '[' *> parseWhitespace *> 
  ((parseChar ']' *> pure []) <|> 
   ((:) <$> parseMechanic <*> parseMechanicListTail <* parseChar ']'))

-- BNF: <number-list-tail> ::= "" | "," <whitespace> <number> <number-list-tail>
parseNumberListTail :: Parser [Int]
parseNumberListTail = 
  parseWhitespace *> 
  ((parseChar ',' *> parseWhitespace *> ((:) <$> parseNumber <*> parseNumberListTail)) <|> 
   pure [])

-- BNF: <number-list> ::= "[" <whitespace> "]" | "[" <whitespace> <number> <number-list-tail> "]"
parseNumberList :: Parser [Int]
parseNumberList = 
  parseChar '[' *> parseWhitespace *> 
  ((parseChar ']' *> pure []) <|> 
   ((:) <$> parseNumber <*> parseNumberListTail <* parseChar ']'))

-- BNF: <car> ::= "Car" <whitespace1> <quoted-string> <whitespace1> <driver> <whitespace1> <mechanic-list> <whitespace1> <number-list>
parseCar :: Parser Lib1.Car
parseCar = 
  Lib1.Car
  <$> (parseString "Car" *> parseWhitespace1 *> parseQuotedString)
  <*> (parseWhitespace1 *> parseDriver)
  <*> (parseWhitespace1 *> parseMechanicList)
  <*> (parseWhitespace1 *> parseNumberList)

-- ============================================================================
-- Command Parsing
-- ============================================================================

-- BNF: <add-car> ::= "add_car" <whitespace1> <car>
parseAddCar :: Parser Lib1.Command
parseAddCar = Lib1.AddCar <$> (parseString "add_car" *> parseWhitespace1 *> parseCar)

-- BNF: <filter-by-number> ::= "filter_by_number" <whitespace1> <quoted-string>
parseFilterByNumber :: Parser Lib1.Command
parseFilterByNumber = Lib1.FilterByNumber <$> (parseString "filter_by_number" *> parseWhitespace1 *> parseQuotedString)

-- BNF: <add-mechanic> ::= "add_mechanic" <whitespace1> <quoted-string> <whitespace1> <quoted-string> <whitespace1> <number>
parseAddMechanic :: Parser Lib1.Command
parseAddMechanic = 
  Lib1.AddMechanic
  <$> (parseString "add_mechanic" *> parseWhitespace1 *> parseQuotedString)
  <*> (parseWhitespace1 *> parseQuotedString)
  <*> (parseWhitespace1 *> parseNumber)

-- BNF: <calculate-average-lap-time> ::= "calculate_average_lap_time" <whitespace1> <car>
parseCalculateAverageLapTime :: Parser Lib1.Command
parseCalculateAverageLapTime = Lib1.CalculateAverageLapTime <$> (parseString "calculate_average_lap_time" *> parseWhitespace1 *> parseCar)

-- BNF: <dump> ::= "dump_examples"
parseDump :: Parser Lib1.Command
parseDump = parseString "dump_examples" *> pure (Lib1.Dump Lib1.Examples)

-- BNF: <simple-command> ::= <add-car> | <filter-by-number> | <add-mechanic> | <calculate-average-lap-time> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = 
  parseAddCar <|>
  parseFilterByNumber <|>
  parseAddMechanic <|>
  parseCalculateAverageLapTime <|>
  parseDump

-- BNF: <command> ::= <simple-command> | <simple-command> ";" <command>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence = do
  firstCmd <- parseSimpleCommand
  _ <- parseWhitespace
  input <- getInput
  case input of
    (';':rest) -> do
      putInput rest
      _ <- parseWhitespace
      nextCmd <- parseSequence
      return (Lib1.Sequence firstCmd nextCmd)
    _ -> return firstCmd

-- | Parses user's input using ExceptT + State monad.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- ============================================================================
-- Arbitrary Instances for QuickCheck
-- ============================================================================

-- | Generate arbitrary alphanumeric strings (avoiding quotes and special chars)
arbitraryAlphaNum :: Gen String
arbitraryAlphaNum = do
  len <- elements [1..10]
  mapM (\_ -> elements (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9'])) [1..len]

-- | Generate arbitrary positive integers
arbitraryPositiveInt :: Gen Int
arbitraryPositiveInt = elements [1..100]

-- | Arbitrary Driver
instance Arbitrary Lib1.Driver where
  arbitrary :: Gen Lib1.Driver
  arbitrary = Lib1.Driver 
    <$> arbitraryAlphaNum 
    <*> arbitraryAlphaNum 
    <*> arbitraryPositiveInt

-- | Arbitrary Mechanic
instance Arbitrary Lib1.Mechanic where
  arbitrary :: Gen Lib1.Mechanic
  arbitrary = Lib1.Mechanic 
    <$> arbitraryAlphaNum 
    <*> arbitraryPositiveInt

-- | Arbitrary Car
instance Arbitrary Lib1.Car where
  arbitrary :: Gen Lib1.Car
  arbitrary = Lib1.Car 
    <$> arbitraryAlphaNum 
    <*> arbitrary 
    <*> listOf arbitrary 
    <*> listOf arbitraryPositiveInt

-- | Arbitrary Dumpable
instance Arbitrary Lib1.Dumpable where
  arbitrary :: Gen Lib1.Dumpable
  arbitrary = pure Lib1.Examples

-- | Arbitrary Command
instance Arbitrary Lib1.Command where
  arbitrary :: Gen Lib1.Command
  arbitrary = sized genCommand
    where
      genCommand :: Int -> Gen Lib1.Command
      genCommand n
        | n <= 0 = genSimpleCommand
        | otherwise = oneof [genSimpleCommand, genSequence (n - 1)]
      
      genSimpleCommand :: Gen Lib1.Command
      genSimpleCommand = oneof
        [ Lib1.AddCar <$> arbitrary
        , Lib1.FilterByNumber <$> arbitraryAlphaNum
        , Lib1.AddMechanic <$> arbitraryAlphaNum <*> arbitraryAlphaNum <*> arbitraryPositiveInt
        , Lib1.CalculateAverageLapTime <$> arbitrary
        , Lib1.Dump <$> arbitrary
        ]
      
      -- Right-associative: left side is always simple, right side can be command
      genSequence :: Int -> Gen Lib1.Command
      genSequence n = Lib1.Sequence <$> genSimpleCommand <*> genCommand n

-- ============================================================================
-- Free Monad DSL
-- ============================================================================

-- | Result type for command execution
data ExecutionResult
  = ResultOk String
  | ResultCar (Maybe Lib1.Car)
  | ResultAverage (Maybe Double)
  | ResultExamples [String]
  deriving (Show, Eq)

-- | DSL functor for commands - one constructor per Lib1.Command
data CommandDSL next
  = AddCarDSL Lib1.Car (ExecutionResult -> next)
  | FilterByNumberDSL String (ExecutionResult -> next)
  | AddMechanicDSL String String Int (ExecutionResult -> next)
  | CalculateAverageLapTimeDSL Lib1.Car (ExecutionResult -> next)
  | DumpExamplesDSL (ExecutionResult -> next)
  deriving Functor

-- | Free monad program type
type CommandProgram = Free CommandDSL

-- | DSL method for AddCar
addCar :: Lib1.Car -> CommandProgram ExecutionResult
addCar car = liftF $ AddCarDSL car id

-- | DSL method for FilterByNumber  
filterByNumber :: String -> CommandProgram ExecutionResult
filterByNumber num = liftF $ FilterByNumberDSL num id

-- | DSL method for AddMechanic
addMechanic :: String -> String -> Int -> CommandProgram ExecutionResult
addMechanic carNum name age = liftF $ AddMechanicDSL carNum name age id

-- | DSL method for CalculateAverageLapTime
calculateAverageLapTime :: Lib1.Car -> CommandProgram ExecutionResult
calculateAverageLapTime car = liftF $ CalculateAverageLapTimeDSL car id

-- | DSL method for Dump Examples
dumpExamples :: CommandProgram ExecutionResult
dumpExamples = liftF $ DumpExamplesDSL id

-- ============================================================================
-- Shared Execution Logic (Pure)
-- ============================================================================

-- | State type matching Lib3.State
data LocalState = LocalState {
  stateCars :: [Lib1.Car]
} deriving (Show, Eq)

emptyLocalState :: LocalState
emptyLocalState = LocalState { stateCars = [] }

-- | Pure command execution - shared between local interpreter and server
executeCommandPure :: Lib1.Command -> LocalState -> (ExecutionResult, LocalState)
executeCommandPure (Lib1.AddCar car) state =
  let newState = state { stateCars = stateCars state ++ [car] }
  in (ResultOk $ "Added car: " ++ Lib1.carNumber car, newState)

executeCommandPure (Lib1.FilterByNumber num) state =
  let result = filter (\c -> Lib1.carNumber c == num) (stateCars state)
  in case result of
    (car:_) -> (ResultCar (Just car), state)
    [] -> (ResultCar Nothing, state)

executeCommandPure (Lib1.AddMechanic carNum name age) state =
  let newCars = map (\car -> 
          if Lib1.carNumber car == carNum
          then car { Lib1.mechanics = Lib1.mechanics car ++ [Lib1.Mechanic name age] }
          else car
        ) (stateCars state)
  in (ResultOk $ "Added mechanic " ++ name ++ " to car " ++ carNum, state { stateCars = newCars })

executeCommandPure (Lib1.CalculateAverageLapTime car) state =
  let times = Lib1.lapTimesS car
  in if null times
     then (ResultAverage Nothing, state)
     else let avg = fromIntegral (sum times) / fromIntegral (length times) :: Double
          in (ResultAverage (Just avg), state)

executeCommandPure (Lib1.Sequence cmd1 cmd2) state =
  let (_, state1) = executeCommandPure cmd1 state
  in executeCommandPure cmd2 state1

executeCommandPure (Lib1.Dump Lib1.Examples) state =
  (ResultExamples $ map Lib2.toCliCommand Lib1.examples, state)

-- ============================================================================
-- Local State Interpreter (Stateful)
-- ============================================================================

-- | Interpret a command program using local State monad
interpretLocal :: CommandProgram a -> State LocalState a
interpretLocal (Pure a) = return a
interpretLocal (Free (AddCarDSL car next)) = do
  state <- Control.Monad.Trans.State.Strict.get
  let (result, newState) = executeCommandPure (Lib1.AddCar car) state
  Control.Monad.Trans.State.Strict.put newState
  interpretLocal (next result)
interpretLocal (Free (FilterByNumberDSL num next)) = do
  state <- Control.Monad.Trans.State.Strict.get
  let (result, newState) = executeCommandPure (Lib1.FilterByNumber num) state
  Control.Monad.Trans.State.Strict.put newState
  interpretLocal (next result)
interpretLocal (Free (AddMechanicDSL carNum name age next)) = do
  state <- Control.Monad.Trans.State.Strict.get
  let (result, newState) = executeCommandPure (Lib1.AddMechanic carNum name age) state
  Control.Monad.Trans.State.Strict.put newState
  interpretLocal (next result)
interpretLocal (Free (CalculateAverageLapTimeDSL car next)) = do
  state <- Control.Monad.Trans.State.Strict.get
  let (result, newState) = executeCommandPure (Lib1.CalculateAverageLapTime car) state
  Control.Monad.Trans.State.Strict.put newState
  interpretLocal (next result)
interpretLocal (Free (DumpExamplesDSL next)) = do
  state <- Control.Monad.Trans.State.Strict.get
  let (result, newState) = executeCommandPure (Lib1.Dump Lib1.Examples) state
  Control.Monad.Trans.State.Strict.put newState
  interpretLocal (next result)

-- | Run a local program from initial state
runLocal :: CommandProgram a -> (a, LocalState)
runLocal program = runState (interpretLocal program) emptyLocalState

-- ============================================================================
-- HTTP Client Interpreter (Stateless)
-- ============================================================================

-- | Convert DSL command to CLI string for HTTP transport
dslToCliCommand :: CommandDSL a -> String
dslToCliCommand (AddCarDSL car _) = Lib2.toCliCommand (Lib1.AddCar car)
dslToCliCommand (FilterByNumberDSL num _) = Lib2.toCliCommand (Lib1.FilterByNumber num)
dslToCliCommand (AddMechanicDSL carNum name age _) = Lib2.toCliCommand (Lib1.AddMechanic carNum name age)
dslToCliCommand (CalculateAverageLapTimeDSL car _) = Lib2.toCliCommand (Lib1.CalculateAverageLapTime car)
dslToCliCommand (DumpExamplesDSL _) = Lib2.toCliCommand (Lib1.Dump Lib1.Examples)

-- | Send command to server and get response
sendToServer :: String -> String -> IO String
sendToServer serverUrl cmdStr = do
  initReq <- parseRequest serverUrl
  let req = setRequestMethod "POST" $ setRequestBodyLBS (LBS.pack cmdStr) initReq
  response <- httpLBS req
  return $ LBS.unpack $ getResponseBody response

-- | Parse server response into ExecutionResult
parseServerResponse :: String -> ExecutionResult
parseServerResponse resp = ResultOk resp

-- | Interpret a command program by sending commands to HTTP server (stateless)
interpretHttp :: String -> CommandProgram a -> IO a
interpretHttp _ (Pure a) = return a
interpretHttp serverUrl (Free dsl) = do
  let cmdStr = dslToCliCommand dsl
  response <- sendToServer serverUrl cmdStr
  let result = parseServerResponse response
  case dsl of
    AddCarDSL _ next -> interpretHttp serverUrl (next result)
    FilterByNumberDSL _ next -> interpretHttp serverUrl (next result)
    AddMechanicDSL _ _ _ next -> interpretHttp serverUrl (next result)
    CalculateAverageLapTimeDSL _ next -> interpretHttp serverUrl (next result)
    DumpExamplesDSL next -> interpretHttp serverUrl (next result)
