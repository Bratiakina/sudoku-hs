module Location where

import Data.Hashable
import Data.List.NonEmpty

data Point = P1 | P2 | P3 | P4 | P5 | P6 | P7 | P8 | P9 deriving (Enum, Eq, Ord)

instance Show Point where
  show P1 = "1"
  show P2 = "2"
  show P3 = "3"
  show P4 = "4"
  show P5 = "5"
  show P6 = "6"
  show P7 = "7"
  show P8 = "8"
  show P9 = "9"

allPoints :: NonEmpty Point
allPoints = P1 :| [P2 .. P9]

data Location = Location Point Point deriving (Eq, Ord)

instance Show Location where
  show (Location x y) = "(" ++ show x ++ "," ++ show y ++ ")"

instance Hashable Location where
  hashWithSalt _ location = hash (show location)

allLocations :: NonEmpty Location
allLocations = do
  x <- allPoints
  y <- allPoints
  return $ Location x y
  
