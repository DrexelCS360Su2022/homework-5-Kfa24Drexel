{-# OPTIONS_GHC -fwarn-tabs #-}

module HaskellIntro where

import Set

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

pow :: (a -> a) -> Int -> a -> a
pow = error "pow not yet defined"

g :: Integer -> Integer
g = error "g not yet defined"

h :: Integer -> Integer
h = error "h not yet defined"

d :: Int -> Integer -> Integer
d = error "d not yet defined"

--
-- Problem 3
--

powerSet = error "powerSet not yet defined"
