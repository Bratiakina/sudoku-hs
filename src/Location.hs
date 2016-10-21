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

newtype Column = Column [Location] deriving (Show)

newtype Row = Row [Location] deriving (Show)

newtype Block = Block [Location] deriving (Show)

instance Group Column where
  points (Column ps) = ps

instance Group Row where
  points (Row ps) = ps

instance Group Block where
  points (Block ps) = ps

segments :: [[Point]]
segments = [[P1, P2, P3], [P4, P5, P6], [P7, P8, P9]]

columns :: [Column]
columns = do
    columnBuilder  <- [ Location x | x <- allPoints ]
    return (Column $ columnBuilder <$> allPoints)

rows :: [Row]
rows = do
    rowBuilder <- [ (\y x -> Location x y) y | y <- allPoints ]
    return (Row $ rowBuilder <$> allPoints)

blocks :: [Block]
blocks = do
  xs <- segments
  ys <- segments
  return $ Block $ do
    x <- xs
    y <- ys
    return $ Location x y




























