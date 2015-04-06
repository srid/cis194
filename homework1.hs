module Homework1 where
import Data.Char

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0    = []
    | otherwise = map charToInteger (show n)

charToInteger :: Char -> Integer
charToInteger = toInteger . Data.Char.digitToInt

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherRev . reverse

doubleEveryOtherRev :: [Integer] -> [Integer]
doubleEveryOtherRev [] = []
doubleEveryOtherRev (x:[]) = [x]
doubleEveryOtherRev (x:y:xs) = [x, y*2] ++ doubleEveryOtherRev xs
                               
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = sumDigitsSolo x + sumDigits xs
                   where sumDigitsSolo = sum . toDigits

validate :: Integer -> Bool
validate = (==0) . (`rem` 10) . sumDigits . doubleEveryOther . toDigits
