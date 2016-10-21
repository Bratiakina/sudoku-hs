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

instance Enum Row where
  toEnum y = Row $ (\x -> Location x (toEnum y)) <$> allPoints
  fromEnum (Row ((Location _ y) :| _)) = fromEnum y
