module Location where

import Data.Hashable

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

allPoints :: [Point]
allPoints = [P1 .. P9]

data Location = Location Point Point deriving (Eq, Ord)

instance Show Location where
	show (Location x y) = "(" ++ show x ++ "," ++ show y ++ ")"
	

instance Hashable Location where
	hashWithSalt _ location = hash (show location)

allLocations :: [Location]
allLocations = [ Location x y | x <- allPoints, y <- allPoints ]

class Group a where
	points :: a -> [Location]

newtype Column = Column { columnLocations :: [Location] }

newtype Row = Row { rowLocations :: [Location] }

newtype Block = Block { blockLocations :: [Location] }

instance Group Column where
	points = columnLocations

instance Group Row where
	points = rowLocations

segments :: [[Point]]
segments = [[P1, P2, P3], [P4, P5, P6], [P7, P8, P9]]






