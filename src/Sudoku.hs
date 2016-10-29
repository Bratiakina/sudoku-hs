module Sudoku where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Location

data Sudoku = Sudoku (Map.Map Location Constraints) deriving (Show)

data Digit = D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 deriving (Enum)

data Constraints = Is Digit | Not (Set.Set Digit)

instance Show Digit where
  show D1 = "1"
  show D2 = "2"
  show D3 = "3"
  show D4 = "4"
  show D5 = "5"
  show D6 = "6"
  show D7 = "7"
  show D8 = "8"
  show D9 = "9"

emptyGame :: Sudoku
emptyGame = Sudoku $ Map.fromList []
