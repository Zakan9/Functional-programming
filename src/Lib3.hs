{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE InstanceSigs #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1
import Control.Applicative (Alternative(..))
import Control.Concurrent.STM (TVar, atomically, readTVar, writeTVar)
import Control.Concurrent (Chan, readChan, newChan, writeChan)
import qualified System.IO.Strict as Strict
import System.IO (writeFile)
import Control.Exception (catch)
import System.IO.Error (IOError)
import Prelude hiding (writeFile)
import Data.List (find)

-- ============================================================================
-- Eq Instances for Lib1 Types (Orphan Instances)
-- ============================================================================

instance Eq Lib1.Car where
    (Lib1.Car n1 d1 m1 l1) == (Lib1.Car n2 d2 m2 l2) = 
        n1 == n2 && d1 == d2 && m1 == m2 && l1 == l2

instance Eq Lib1.Driver where
    (Lib1.Driver n1 nat1 a1) == (Lib1.Driver n2 nat2 a2) = 
        n1 == n2 && nat1 == nat2 && a1 == a2

instance Eq Lib1.Mechanic where
    (Lib1.Mechanic n1 a1) == (Lib1.Mechanic n2 a2) = 
        n1 == n2 && a1 == a2

-- ============================================================================
-- Parser Definition with Functor, Applicative, Alternative
-- ============================================================================

newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser p) = Parser $ \input -> case p input of
        Left err -> Left err
        Right (result, rest) -> Right (f result, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \input -> Right (x, input)
    
    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (Parser pf) <*> (Parser pa) = Parser $ \input -> case pf input of
        Left err -> Left err
        Right (f, rest1) -> case pa rest1 of
            Left err -> Left err
            Right (a, rest2) -> Right (f a, rest2)

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left "No parse"
    
    (<|>) :: Parser a -> Parser a -> Parser a
    (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
        Right result -> Right result
        Left _ -> p2 input

-- ============================================================================
-- Basic Parser Combinators
-- ============================================================================

parseString :: String -> Parser String
parseString expected = Parser $ \input ->
    if take (length expected) input == expected
    then Right (expected, drop (length expected) input)
    else Left $ "Expected '" ++ expected ++ "'"

parseWhitespace :: Parser String
parseWhitespace = Parser $ \input ->
    let (ws, rest) = span isWhitespaceChar input
    in Right (ws, rest)

parseWhitespace1 :: Parser String
parseWhitespace1 = Parser $ \input -> case input of
    [] -> Left "Expected whitespace"
    (c:rest) -> if isWhitespaceChar c
        then let (ws, rest2) = span isWhitespaceChar rest
             in Right (c:ws, rest2)
        else Left "Expected whitespace"

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

parseNumber :: Parser Int
parseNumber = Parser $ \input -> case input of
    [] -> Left "Expected number"
    _ -> case span isDigitChar input of
        ([], _) -> Left "Expected number"
        (digits, rest) -> Right (read digits, rest)

isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

parseChar :: Char -> Parser Char
parseChar expected = Parser $ \input -> case input of
    [] -> Left $ "Expected '" ++ [expected] ++ "'"
    (c:rest) -> if c == expected
        then Right (c, rest)
        else Left $ "Expected '" ++ [expected] ++ "'"

parseQuotedString :: Parser String
parseQuotedString = Parser $ \input -> case input of
    [] -> Left "Expected quoted string"
    ('"':rest) -> 
        let (content, rest2) = span (/= '"') rest
        in case rest2 of
            ('"':rest3) -> Right (content, rest3)
            _ -> Left "Unterminated quoted string"
    _ -> Left "Expected opening quote"

-- ============================================================================
-- Domain Parsing
-- ============================================================================

-- BNF: <driver> ::= "(" "Driver" <whitespace> <quoted-string> <whitespace> <quoted-string> <whitespace> <number> ")"
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

-- BNF: <mechanic> ::= "(" "Mechanic" <whitespace> <quoted-string> <whitespace> <number> ")"
parseMechanic :: Parser Lib1.Mechanic
parseMechanic = 
    (\_ _ name _ age _ -> Lib1.Mechanic name age)
    <$> parseChar '('
    <*> parseString "Mechanic"
    <*> (parseWhitespace1 *> parseQuotedString)
    <*> parseWhitespace1
    <*> parseNumber
    <*> parseChar ')'

-- BNF: <mechanic-list> ::= "[" <whitespace> "]" | "[" <whitespace> <mechanic> <mechanic-list-tail> "]"
parseMechanicList :: Parser [Lib1.Mechanic]
parseMechanicList = 
    parseChar '[' *> parseWhitespace *> 
    ((parseChar ']' *> pure []) <|> 
     ((:) <$> parseMechanic <*> parseMechanicListTail <* parseChar ']'))

parseMechanicListTail :: Parser [Lib1.Mechanic]
parseMechanicListTail = 
    parseWhitespace *> 
    ((parseChar ',' *> parseWhitespace *> ((:) <$> parseMechanic <*> parseMechanicListTail)) <|> 
     pure [])

-- BNF: <number-list> ::= "[" <whitespace> "]" | "[" <whitespace> <number> <number-list-tail> "]"
parseNumberList :: Parser [Int]
parseNumberList = 
    parseChar '[' *> parseWhitespace *> 
    ((parseChar ']' *> pure []) <|> 
     ((:) <$> parseNumber <*> parseNumberListTail <* parseChar ']'))

parseNumberListTail :: Parser [Int]
parseNumberListTail = 
    parseWhitespace *> 
    ((parseChar ',' *> parseWhitespace *> ((:) <$> parseNumber <*> parseNumberListTail)) <|> 
     pure [])

-- BNF: <car> ::= "Car" <whitespace> <quoted-string> <whitespace> <driver> <whitespace> <mechanic-list> <whitespace> <number-list>
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

-- BNF: <add-car> ::= "add_car" <whitespace> <car>
parseAddCar :: Parser Lib1.Command
parseAddCar = Lib1.AddCar <$> (parseString "add_car" *> parseWhitespace1 *> parseCar)

-- BNF: <filter-by-number> ::= "filter_by_number" <whitespace> <quoted-string>
parseFilterByNumber :: Parser Lib1.Command
parseFilterByNumber = Lib1.FilterByNumber <$> (parseString "filter_by_number" *> parseWhitespace1 *> parseQuotedString)

-- BNF: <add-mechanic> ::= "add_mechanic" <whitespace> <quoted-string> <whitespace> <quoted-string> <whitespace> <number>
parseAddMechanic :: Parser Lib1.Command
parseAddMechanic = 
    Lib1.AddMechanic
    <$> (parseString "add_mechanic" *> parseWhitespace1 *> parseQuotedString)
    <*> (parseWhitespace1 *> parseQuotedString)
    <*> (parseWhitespace1 *> parseNumber)

-- BNF: <calculate-average-lap-time> ::= "calculate_average_lap_time" <whitespace> <car>
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

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence = Parser $ \input -> case runParser parseSimpleCommand input of
    Left err -> Left err
    Right (firstCmd, rest) -> collectCommands [firstCmd] rest
  where
    collectCommands :: [Lib1.Command] -> String -> Either String (Lib1.Command, String)
    collectCommands cmds input' = case runParser parseWhitespace input' of
        Left err -> Left err
        Right (_, r) -> case r of
            (';':r2) -> case runParser (parseWhitespace *> parseSimpleCommand) r2 of
                Left err -> Left err
                Right (cmd, rest2) -> collectCommands (cmds ++ [cmd]) rest2
            _ -> Right (foldl1 Lib1.Sequence cmds, r)

parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- ============================================================================
-- State Definition
-- ============================================================================

data State = State {
    stateCars :: [Lib1.Car]
} deriving (Show, Eq)

emptyState :: State
emptyState = State { stateCars = [] }

-- ============================================================================
-- Business Logic
-- ============================================================================

executeCommand :: Lib1.Command -> State -> IO State
executeCommand (Lib1.AddCar car) state = do
    putStrLn $ "Added car: " ++ Lib1.carNumber car
    return $ state { stateCars = stateCars state ++ [car] }

executeCommand (Lib1.FilterByNumber num) state = do
    case find (\c -> Lib1.carNumber c == num) (stateCars state) of
        Just car -> putStrLn $ "Found car: " ++ show car
        Nothing -> putStrLn $ "Car not found: " ++ num
    return state

executeCommand (Lib1.AddMechanic carNum name age) state = do
    let newCars = map (\car -> 
            if Lib1.carNumber car == carNum
            then car { Lib1.mechanics = Lib1.mechanics car ++ [Lib1.Mechanic name age] }
            else car
            ) (stateCars state)
    putStrLn $ "Added mechanic " ++ name ++ " to car " ++ carNum
    return $ state { stateCars = newCars }

executeCommand (Lib1.CalculateAverageLapTime car) state = do
    let times = Lib1.lapTimesS car
    if null times
    then putStrLn "No lap times available"
    else do
        let avg = fromIntegral (sum times) / fromIntegral (length times) :: Double
        putStrLn $ "Average lap time for car " ++ Lib1.carNumber car ++ ": " ++ show avg ++ "s"
    return state

executeCommand (Lib1.Sequence cmd1 cmd2) state = do
    state1 <- executeCommand cmd1 state
    executeCommand cmd2 state1

executeCommand (Lib1.Dump Lib1.Examples) state = do
    putStrLn "Examples:"
    mapM_ (putStrLn . toCliCommand) Lib1.examples
    return state

execute :: TVar State -> Lib1.Command -> IO ()
execute tvar cmd = do
    oldState <- atomically $ readTVar tvar
    newState <- executeCommand cmd oldState
    atomically $ writeTVar tvar newState

-- ============================================================================
-- State Serialization
-- ============================================================================

stateToCommands :: State -> [Lib1.Command]
stateToCommands state = map Lib1.AddCar (stateCars state)

toCliCommand :: Lib1.Command -> String
toCliCommand (Lib1.AddCar car) = "add_car " ++ carToString car
toCliCommand (Lib1.FilterByNumber num) = "filter_by_number \"" ++ num ++ "\""
toCliCommand (Lib1.AddMechanic carNum name age) = 
    "add_mechanic \"" ++ carNum ++ "\" \"" ++ name ++ "\" " ++ show age
toCliCommand (Lib1.CalculateAverageLapTime car) = "calculate_average_lap_time " ++ carToString car
toCliCommand (Lib1.Sequence cmd1 cmd2) = toCliCommand cmd1 ++ "; " ++ toCliCommand cmd2
toCliCommand (Lib1.Dump Lib1.Examples) = "dump_examples"

carToString :: Lib1.Car -> String
carToString (Lib1.Car num drv mechs times) =
  "Car \"" ++ num ++ "\" " ++ driverToString drv ++ " " ++ 
  mechanicsToString mechs ++ " " ++ timesToString times

driverToString :: Lib1.Driver -> String
driverToString (Lib1.Driver name nat age) =
  "(Driver \"" ++ name ++ "\" \"" ++ nat ++ "\" " ++ show age ++ ")"

mechanicToString :: Lib1.Mechanic -> String
mechanicToString (Lib1.Mechanic name age) =
  "(Mechanic \"" ++ name ++ "\" " ++ show age ++ ")"

mechanicsToString :: [Lib1.Mechanic] -> String
mechanicsToString [] = "[]"
mechanicsToString mechs = "[" ++ mechanicsToStringHelper mechs ++ "]"
  where
    mechanicsToStringHelper [m] = mechanicToString m
    mechanicsToStringHelper (m:ms) = mechanicToString m ++ ", " ++ mechanicsToStringHelper ms
    mechanicsToStringHelper [] = ""

timesToString :: [Int] -> String
timesToString [] = "[]"
timesToString times = "[" ++ timesToStringHelper times ++ "]"
  where
    timesToStringHelper [t] = show t
    timesToStringHelper (t:ts) = show t ++ ", " ++ timesToStringHelper ts
    timesToStringHelper [] = ""

-- ============================================================================
-- Storage Operations
-- ============================================================================

data StorageOp = Save String (Chan ()) | Load (Chan String)

storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = loop
  where
    loop = do
        op <- readChan chan
        case op of
            Save content responseChan -> do
                writeFile "state.txt" content
                writeChan responseChan ()
                loop
            Load responseChan -> do
                content <- Strict.readFile "state.txt" `catch` handleNotFound
                writeChan responseChan content
                loop
    handleNotFound :: IOError -> IO String
    handleNotFound _ = return ""

save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan tvar = do
    state <- atomically $ readTVar tvar
    let commands = stateToCommands state
    let content = unlines $ map toCliCommand commands
    responseChan <- newChan
    writeChan chan (Save content responseChan)
    _ <- readChan responseChan
    return $ Right ()

load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan tvar = do
    responseChan <- newChan
    writeChan chan (Load responseChan)
    content <- readChan responseChan
    if null content || all null (lines content)
    then do
        atomically $ writeTVar tvar emptyState
        return $ Right ()
    else case parseAndExecuteCommands content of
        Left err -> return $ Left err
        Right state -> do
            atomically $ writeTVar tvar state
            return $ Right ()

parseAndExecuteCommands :: String -> Either String State
parseAndExecuteCommands content = 
    let commandLines = filter (not . null) $ lines content
        parsedCommands = map (runParser parseCommand) commandLines
    in case sequence parsedCommands of
        Left err -> Left err
        Right commands -> Right $ foldl applyCommand emptyState (map fst commands)
  where
    applyCommand :: State -> Lib1.Command -> State
    applyCommand state (Lib1.AddCar car) = state { stateCars = stateCars state ++ [car] }
    applyCommand state (Lib1.AddMechanic carNum name age) = 
        let newCars = map (\car -> 
                if Lib1.carNumber car == carNum
                then car { Lib1.mechanics = Lib1.mechanics car ++ [Lib1.Mechanic name age] }
                else car
                ) (stateCars state)
        in state { stateCars = newCars }
    applyCommand state (Lib1.Sequence cmd1 cmd2) = applyCommand (applyCommand state cmd1) cmd2
    applyCommand state _ = state