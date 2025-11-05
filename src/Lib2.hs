{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where

import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- | Parses user's input.
parseCommand :: Parser Lib1.Command
parseCommand = parseSequence

-- BNF: <command> ::= <simple-command> | <sequence>
-- BNF: <sequence> ::= <simple-command> ";" <command>
parseSequence :: Parser Lib1.Command
parseSequence input = case parseSimpleCommand input of
  Left err -> Left err
  Right (firstCmd, rest) -> collect [firstCmd] rest
  where
    collect :: [Lib1.Command] -> String -> Either ErrorMsg (Lib1.Command, String)
    collect cmds input' =
      case parseWhitespace input' of
        Left err -> Left err
        Right (_, r) -> case r of
          (';':r2) -> 
            case parseWhitespace r2 of
              Left err -> Left err
              Right (_, r3) -> case parseSimpleCommand r3 of
                Left err -> Left err
                Right (cmd2, rest2) -> collect (cmds ++ [cmd2]) rest2
          _ -> Right (foldl1 Lib1.Sequence cmds, r)

-- BNF: <simple-command> ::= <add-car> | <filter-by-number> | <add-mechanic> | 
--                           <calculate-average-lap-time> | <dump>
parseSimpleCommand :: Parser Lib1.Command
parseSimpleCommand = orElse (orElse (orElse (orElse
  parseAddCar
  parseFilterByNumber)
  parseAddMechanic)
  parseCalculateAverageLapTime)
  parseDump

-- BNF: <add-car> ::= "add_car" <whitespace> <car>
parseAddCar :: Parser Lib1.Command
parseAddCar input = 
  case and3 (parseString "add_car") parseWhitespace1 parseCar input of
    Left err -> Left err
    Right (((_, _), car), rest) -> Right (Lib1.AddCar car, rest)

-- BNF: <filter-by-number> ::= "filter_by_number" <whitespace> <quoted-string>
parseFilterByNumber :: Parser Lib1.Command
parseFilterByNumber input =
  case and3 (parseString "filter_by_number") parseWhitespace1 parseQuotedString input of
    Left err -> Left err
    Right (((_, _), num), rest) -> Right (Lib1.FilterByNumber num, rest)

-- BNF: <add-mechanic> ::= "add_mechanic" <whitespace> <quoted-string> <whitespace> 
--                         <quoted-string> <whitespace> <number>
parseAddMechanic :: Parser Lib1.Command
parseAddMechanic input =
  case and3 (parseString "add_mechanic") parseWhitespace1 parseQuotedString input of
    Left err -> Left err
    Right (((_, _), carNum), rest) -> 
      case parseWhitespace1 rest of
        Left err -> Left err
        Right (_, rest2) -> case parseQuotedString rest2 of
          Left err -> Left err
          Right (name, rest3) -> case parseWhitespace1 rest3 of
            Left err -> Left err
            Right (_, rest4) -> case parseNumber rest4 of
              Left err -> Left err
              Right (age, rest5) -> Right (Lib1.AddMechanic carNum name age, rest5)

-- BNF: <calculate-average-lap-time> ::= "calculate_average_lap_time" <whitespace> <car>
parseCalculateAverageLapTime :: Parser Lib1.Command
parseCalculateAverageLapTime input =
  case and3 (parseString "calculate_average_lap_time") parseWhitespace1 parseCar input of
    Left err -> Left err
    Right (((_, _), car), rest) -> Right (Lib1.CalculateAverageLapTime car, rest)

-- BNF: <dump> ::= "dump_examples"
parseDump :: Parser Lib1.Command
parseDump input =
  case parseString "dump_examples" input of
    Left err -> Left err
    Right (_, rest) -> Right (Lib1.Dump Lib1.Examples, rest)

-- BNF: <car> ::= "Car" <whitespace> <quoted-string> <whitespace> <driver> <whitespace> <mechanic-list> <whitespace> <number-list>
parseCar :: Parser Lib1.Car
parseCar input = 
  case and3 (parseString "Car") parseWhitespace1 parseQuotedString input of
    Left err -> Left err
    Right (((_, _), carNum), rest) ->
      case parseWhitespace1 rest of
        Left err -> Left err
        Right (_, rest2) -> case parseDriver rest2 of
          Left err -> Left err
          Right (drv, rest3) -> case parseWhitespace1 rest3 of
            Left err -> Left err
            Right (_, rest4) -> case parseMechanicListBracket rest4 of
              Left err -> Left err
              Right (mechs, rest5) -> case parseWhitespace1 rest5 of
                Left err -> Left err
                Right (_, rest6) -> case parseNumberListBracket rest6 of
                  Left err -> Left err
                  Right (times, rest7) -> Right (Lib1.Car carNum drv mechs times, rest7)

-- BNF: <driver> ::= "(" "Driver" <whitespace> <quoted-string> <whitespace> <quoted-string> <whitespace> <number> ")"
parseDriver :: Parser Lib1.Driver
parseDriver input = case parseChar '(' input of
  Left err -> Left err
  Right (_, rest) -> 
    case and3 (parseString "Driver") parseWhitespace1 parseQuotedString rest of
      Left err -> Left err
      Right (((_, _), name), rest2) ->
        case parseWhitespace1 rest2 of
          Left err -> Left err
          Right (_, rest3) -> case parseQuotedString rest3 of
            Left err -> Left err
            Right (nat, rest4) -> case parseWhitespace1 rest4 of
              Left err -> Left err
              Right (_, rest5) -> case parseNumber rest5 of
                Left err -> Left err
                Right (age, rest6) -> case parseChar ')' rest6 of
                  Left err -> Left err
                  Right (_, rest7) -> Right (Lib1.Driver name nat age, rest7)

-- BNF: <mechanic> ::= "(" "Mechanic" <whitespace> <quoted-string> <whitespace> <number> ")"
parseMechanic :: Parser Lib1.Mechanic
parseMechanic input = case parseChar '(' input of
  Left err -> Left err
  Right (_, rest) ->
    case and3 (parseString "Mechanic") parseWhitespace1 parseQuotedString rest of
      Left err -> Left err
      Right (((_, _), name), rest2) ->
        case parseWhitespace1 rest2 of
          Left err -> Left err
          Right (_, rest3) -> case parseNumber rest3 of
            Left err -> Left err
            Right (age, rest4) -> case parseChar ')' rest4 of
              Left err -> Left err
              Right (_, rest5) -> Right (Lib1.Mechanic name age, rest5)

-- BNF: <mechanic-list> ::= "[" <whitespace> "]" | "[" <whitespace> <mechanic> <mechanic-list-tail> "]"
parseMechanicListBracket :: Parser [Lib1.Mechanic]
parseMechanicListBracket input = case parseChar '[' input of
  Left err -> Left err
  Right (_, rest) -> case parseWhitespace rest of
    Left err -> Left err
    Right (_, rest2) -> case parseChar ']' rest2 of
      Right (_, rest3) -> Right ([], rest3)
      Left _ -> case parseMechanic rest2 of
        Left err -> Left err
        Right (mech, rest3) -> case parseMechanicListTail rest3 of
          Left err -> Left err
          Right (mechs, rest4) -> case parseChar ']' rest4 of
            Left err -> Left err
            Right (_, rest5) -> Right (mech:mechs, rest5)

-- BNF: <mechanic-list-tail> ::= <whitespace> | <whitespace> "," <whitespace> <mechanic> <mechanic-list-tail>
parseMechanicListTail :: Parser [Lib1.Mechanic]
parseMechanicListTail input = case parseWhitespace input of
  Left err -> Left err
  Right (_, rest) -> case parseChar ',' rest of
    Left _ -> Right ([], rest)
    Right (_, rest2) -> case parseWhitespace rest2 of
      Left err -> Left err
      Right (_, rest3) -> case parseMechanic rest3 of
        Left err -> Left err
        Right (mech, rest4) -> case parseMechanicListTail rest4 of
          Left err -> Left err
          Right (mechs, rest5) -> Right (mech:mechs, rest5)

-- BNF: <number-list> ::= "[" <whitespace> "]" | "[" <whitespace> <number> <number-list-tail> "]"
parseNumberListBracket :: Parser [Int]
parseNumberListBracket input = case parseChar '[' input of
  Left err -> Left err
  Right (_, rest) -> case parseWhitespace rest of
    Left err -> Left err
    Right (_, rest2) -> case parseChar ']' rest2 of
      Right (_, rest3) -> Right ([], rest3)
      Left _ -> case parseNumber rest2 of
        Left err -> Left err
        Right (num, rest3) -> case parseNumberListTail rest3 of
          Left err -> Left err
          Right (nums, rest4) -> case parseChar ']' rest4 of
            Left err -> Left err
            Right (_, rest5) -> Right (num:nums, rest5)

-- BNF: <number-list-tail> ::= <whitespace> | <whitespace> "," <whitespace> <number> <number-list-tail>
parseNumberListTail :: Parser [Int]
parseNumberListTail input = case parseWhitespace input of
  Left err -> Left err
  Right (_, rest) -> case parseChar ',' rest of
    Left _ -> Right ([], rest)
    Right (_, rest2) -> case parseWhitespace rest2 of
      Left err -> Left err
      Right (_, rest3) -> case parseNumber rest3 of
        Left err -> Left err
        Right (num, rest4) -> case parseNumberListTail rest4 of
          Left err -> Left err
          Right (nums, rest5) -> Right (num:nums, rest5)

-- BNF: <quoted-string> ::= "\"" <string-content> "\""
parseQuotedString :: Parser String
parseQuotedString [] = Left "Expected quoted string"
parseQuotedString ('"':rest) = 
  let (content, rest2) = span (/= '"') rest
  in case rest2 of
    ('"':rest3) -> Right (content, rest3)
    _ -> Left "Unterminated quoted string"
parseQuotedString _ = Left "Expected opening quote"

-- BNF: <number> ::= <digit> | <digit> <number>
parseNumber :: Parser Int
parseNumber [] = Left "Expected number"
parseNumber input = 
  case span isDigitChar input of
    ([], _) -> Left "Expected number"
    (digits, rest) -> Right (read digits, rest)

-- BNF: <char> ::= any single character
parseChar :: Char -> Parser Char
parseChar expected [] = Left $ "Expected '" ++ [expected] ++ "'"
parseChar expected (c:rest)
  | c == expected = Right (c, rest)
  | otherwise = Left $ "Expected '" ++ [expected] ++ "'"

-- BNF: <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
isDigitChar :: Char -> Bool
isDigitChar c = c >= '0' && c <= '9'

-- BNF: <string> ::= <char> | <char> <string>
parseString :: String -> Parser String
parseString expected input
  | take (length expected) input == expected = Right (expected, drop (length expected) input)
  | otherwise = Left $ "Expected '" ++ expected ++ "'"

-- BNF: <whitespace> ::= " " | "\t" | "\n" | <whitespace> <whitespace>
parseWhitespace :: Parser String
parseWhitespace input = 
  let (ws, rest) = span isWhitespaceChar input
  in Right (ws, rest)

-- Parse at least one whitespace character
parseWhitespace1 :: Parser String
parseWhitespace1 [] = Left "Expected whitespace"
parseWhitespace1 (c:rest)
  | isWhitespaceChar c = 
      let (ws, rest2) = span isWhitespaceChar rest
      in Right (c:ws, rest2)
  | otherwise = Left "Expected whitespace"

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t' || c == '\n'

-- Parser combinators
orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right result -> Right result
  Left _ -> p2 input

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = case p1 input of
  Left err -> Left err
  Right (r1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (r2, rest2) -> Right ((r1, r2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser ((a, b), c)
and3 p1 p2 p3 = and2 (and2 p1 p2) p3

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (((a, b), c), d)
and4 p1 p2 p3 p4 = and2 (and3 p1 p2 p3) p4

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser ((((a, b), c), d), e)
and5 p1 p2 p3 p4 p5 = and2 (and4 p1 p2 p3 p4) p5

-- Process function
process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

-- ToCliCommand instance
class ToCliCommand a where
  toCliCommand :: a -> String

instance ToCliCommand Lib1.Command where
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

instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.AddCar c1) == (Lib1.AddCar c2) = c1 == c2
  (Lib1.FilterByNumber n1) == (Lib1.FilterByNumber n2) = n1 == n2
  (Lib1.AddMechanic cn1 n1 a1) == (Lib1.AddMechanic cn2 n2 a2) = 
    cn1 == cn2 && n1 == n2 && a1 == a2
  (Lib1.CalculateAverageLapTime c1) == (Lib1.CalculateAverageLapTime c2) = c1 == c2
  (Lib1.Sequence cmd1a cmd1b) == (Lib1.Sequence cmd2a cmd2b) = cmd1a == cmd2a && cmd1b == cmd2b
  (Lib1.Dump d1) == (Lib1.Dump d2) = d1 == d2
  _ == _ = False

instance Eq Lib1.Car where
  (Lib1.Car n1 d1 m1 l1) == (Lib1.Car n2 d2 m2 l2) = 
    n1 == n2 && d1 == d2 && m1 == m2 && l1 == l2

instance Eq Lib1.Driver where
  (Lib1.Driver n1 nat1 a1) == (Lib1.Driver n2 nat2 a2) = 
    n1 == n2 && nat1 == nat2 && a1 == a2

instance Eq Lib1.Mechanic where
  (Lib1.Mechanic n1 a1) == (Lib1.Mechanic n2 a2) = n1 == n2 && a1 == a2

instance Eq Lib1.Dumpable where
  Lib1.Examples == Lib1.Examples = True