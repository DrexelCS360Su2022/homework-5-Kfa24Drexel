{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set
import GHC.Num (integerToInt)

-- Load this file into GHCi (say, with `ghci HaskellIntro.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

isThisWorking :: String
isThisWorking = "Yes"

--
-- Problem 1
--

lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - lastDigit n) `div` 10

toDigits :: Integer -> [Integer]
toDigits n
    | n <= 0 = []
    | otherwise = toDigits (dropLastDigit n) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther x = reverseList (doubleEveryOtherHelper x)

doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:xs)
    | odd (length xs) = doubleEveryOtherHelper xs ++ [x * 2]
    | otherwise = doubleEveryOtherHelper xs ++ [x]

reverseList :: [Integer] -> [Integer]
reverseList [] = []
reverseList (x:xs) = reverseList xs ++ [x]

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x:xs) = sumDigits xs + digitAdder x

digitAdder :: Integer -> Integer
digitAdder n
    | n < 10 = n
    | otherwise = digitAdder (dropLastDigit n) + lastDigit n

validate :: Integer -> Bool
validate n
    | lastDigit (sumDigits (doubleEveryOther (toDigits n))) == 0 = True
    | otherwise = False

--
-- Problem 2
--

simple :: a -> a
simple x = x

pow :: (a -> a) -> Int -> a -> a
pow f n
    | n < 1 = simple
    | n == 1 = f
    | otherwise = f.pow f (n - 1)

g :: Integer -> Integer
g 0 = 0
g n = n - pow g 2 (n-1)

h :: Integer -> Integer
h 0 = 0
h n = n - pow h 3 (n-1)

d :: Integer -> Integer -> Integer
d i 0 = 0
d 2 n = g n
d 3 n = h n
d i n = 0




--
-- Problem 3
--

powerSet :: Ord a => Set a -> Set (Set a)
powerSet xs
    | isEmpty xs = singleton xs
    | otherwise = singleton xs