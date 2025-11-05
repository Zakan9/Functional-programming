module Lib1
    ( examples, Command(..), Dumpable(..)
    , Car(..), Driver(..), Mechanic(..)
    ) where

data Car = Car
  { carNumber :: String
  , driver :: Driver
  , mechanics :: [Mechanic]
  , lapTimesS :: [Int] 
  } deriving Show

data Driver = Driver
  { driverName :: String
  , driverNationality :: String
  , driverAge :: Int
  } deriving Show

data Mechanic = Mechanic
  { mechanicName :: String
  , mechanicAge :: Int
  } deriving Show

data Command
  = AddCar Car
  | FilterByNumber String
  | AddMechanic String String Int
  | Sequence Command Command
  | CalculateAverageLapTime Car
  | Dump Dumpable
  deriving Show

data Dumpable 
  = Examples
  deriving Show

car0 :: Car
car0 = Car
  { carNumber = "44"
  , driver = Driver { driverName = "Lewis Hamilton", driverNationality = "UK", driverAge = 38 }
  , mechanics = []
  , lapTimesS = [63, 64]
  }

car1 :: Car
car1 = Car
  { carNumber = "1"  
  , driver = Driver { driverName = "Max Verstappen", driverNationality = "Netherlands", driverAge = 26 }
  , mechanics =
      [ Mechanic { mechanicName = "John", mechanicAge = 40 }
      , Mechanic { mechanicName = "Maria", mechanicAge = 29 }
      ]
  ,    lapTimesS = [60, 58]
  }

car2 :: Car
car2 = Car
  { carNumber = "16"
  , driver = Driver { driverName = "Charles Leclerc", driverNationality = "Monaco", driverAge = 25 }
  , mechanics =
      [ Mechanic { mechanicName = "Luca", mechanicAge = 33 } ]
  ,    lapTimesS = [59, 59, 61]
  }

example0 :: Command
example0 = AddCar car0

example1 :: Command
example1 = FilterByNumber "16"

example2 :: Command
example2 = AddMechanic "44" "Sophia" 27

example3 :: Command
example3 = CalculateAverageLapTime car2

example4 :: Command
example4 = Sequence 
  (Sequence 
    (AddCar car0)
    (AddCar car1)
  )
  (AddCar car2)

examples :: [Command]
examples = [example0, example1, example2, example3, example4, Dump Examples]