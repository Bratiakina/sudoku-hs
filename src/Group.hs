module Group where

import Data.Hashable
import Data.List.NonEmpty
import Location

-- A group of locations where a digit can only be placed once
class Group a where
  points :: a -> NonEmpty Location

-- The types of groups
newtype Column = Column (NonEmpty Location) deriving (Show)
newtype Row = Row (NonEmpty Location) deriving (Show)
newtype Block = Block (NonEmpty Location) deriving (Show)

-- And their Group instances
instance Group Column where
  points (Column ps) = ps

instance Group Row where
  points (Row ps) = ps

instance Group Block where
  points (Block ps) = ps

-- Enum instances
instance Enum Column where
  toEnum x = Column $ (Location $ toEnum x) <$> allPoints
  fromEnum (Column ((Location x _) :| _)) = fromEnum x

instance Bounded Column where
  minBound = toEnum 0
  maxBound = toEnum 8

instance Enum Row where
  toEnum y = Row $ (\x -> Location x (toEnum y)) <$> allPoints
  fromEnum (Row ((Location _ y) :| _)) = fromEnum y

instance Bounded Row where
  minBound = toEnum 0
  maxBound = toEnum 8

instance Enum Block where
  toEnum i = Block $ do
    x <- segments . column $ i
    y <- segments . row $ i
    return $ (Location x y)
    where
      column :: Int -> Int
      column i = (div i 3)

      row :: Int -> Int
      row i = (mod i 3)
  
      segments :: Int -> (NonEmpty Point)
      segments 0 = (P1 :| [P2, P3])
      segments 1 = (P4 :| [P5, P6])
      segments 2 = (P7 :| [P8, P9])
      segments _ = undefined
      
  fromEnum (Block (p :| _)) = (3 *(row p)) + (column p)
    where
      row (Location _ y) = div (fromEnum y) 3
      column (Location x _) = div (fromEnum x) 3
  
instance Bounded Block where
  minBound = toEnum 0
  maxBound = toEnum 8
