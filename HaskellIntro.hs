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
lastDigit n = n `rem` 10 

dropLastDigit :: Integer -> Integer
dropLastDigit n = (n - (lastDigit n)) `div` 10 

toDigits :: Integer -> [Integer]
toDigits n | n <= 0       = []
           | otherwise = (toDigits (dropLastDigit n)) ++ [lastDigit n]

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther (x:xs) | null xs = [x] 
                        | ((length xs)+1) `mod` 2 /= 0 = x : doubleEveryOther xs 
                        | ((length xs)+1) `mod` 2 == 0 = 2*x : doubleEveryOther xs

sumDigits :: [Integer] -> Integer
sumDigits [] = 0 
sumDigits (x:xs) | (dropLastDigit x) > 0 = sumDigits (toDigits x) 
                 | otherwise = x + (sumDigits xs) 

validate :: Integer -> Bool
validate n | (sumDigits(doubleEveryOther((toDigits n))) `mod` 10 == 0) = True
           | otherwise = False
--
-- Problem 2
--

pow :: (a -> a) -> Int -> a -> a
pow f n | n==0 = f
        | otherwise = powHelper f n f 

powHelper :: (a -> a) -> Int -> (a -> a) -> a -> a
powHelper f n k | n == 1 = k
                | otherwise = powHelper f (n-1) (f.k)

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
