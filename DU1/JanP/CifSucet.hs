module CifSucet where

cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)

totalCifSuc :: Integer -> Integer
totalCifSuc n
    | cifSuc < 10 = cifSuc
    | otherwise = totalCifSuc cifSuc
    where cifSuc = sum $ cifryR n

isJ :: Integer -> Bool
isJ n = totalCifSuc n == 5

jCislaPocet' :: Integer -> Integer -> Integer
jCislaPocet' a b = toInteger $ length $ filter isJ [a..b]

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = (b+4) `div` 9 - (a+3) `div` 9
