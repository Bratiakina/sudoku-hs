module Model.Parser where

import Text.ParserCombinators.ReadP
import Control.Monad.IO.Class
import Model.Sudoku

isNumber :: Char -> Bool
isNumber char = elem char ['1'..'9']

number :: ReadP Char
number = satisfy isNumber

digit :: ReadP Digit
digit = number >>= (\i -> case charToDigit i of
                       Just d -> return d
                       Nothing -> pfail)

charToDigit :: Char -> Maybe Digit
charToDigit '1' = Just D1
charToDigit '2' = Just D2
charToDigit '3' = Just D3
charToDigit '4' = Just D4
charToDigit '5' = Just D5
charToDigit '6' = Just D6
charToDigit '7' = Just D7
charToDigit '8' = Just D8
charToDigit '9' = Just D9
charToDigit _   = Nothing

sudokuParser :: ReadP [[Digit]]
sudokuParser = count 9 (count 9 digit)

readSudoku :: String -> [([[Digit]], String)]
readSudoku = readP_to_S sudokuParser
