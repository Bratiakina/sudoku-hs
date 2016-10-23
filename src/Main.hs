module Main where

import Location
import Group
--import Sudoku

main :: IO ()
main = putStrLn . show $ ([minBound .. maxBound] :: [Block])
