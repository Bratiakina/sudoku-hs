module Main where

import Location
import Sudoku

-- foldl :: (a -> b -> a) -> a -> [b] -> a

main :: IO ()
main = putStrLn . show $ columns