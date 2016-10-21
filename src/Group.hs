module Group where

import Data.Hashable
import Location

-- A group of locations where a digit can only be placed once
class Group a where
  points :: a -> [Location]

-- The types of groups
newtype Column = Column [Location] deriving (Show)
newtype Row = Row [Location] deriving (Show)
newtype Block = Block [Location] deriving (Show)

-- And their Group instances
instance Group Column where
  points (Column ps) = ps

instance Group Row where
  points (Row ps) = ps

instance Group Block where
  points (Block ps) = ps

-- Constants containing all groups by type
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
      where
        segments = [[P1, P2, P3], [P4, P5, P6], [P7, P8, P9]]
