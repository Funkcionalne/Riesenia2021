module CifSucet where

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = if mod (b + 1 - a) 9 > (prve - a) then (div (b + 1 - a) 9) + 1 else div (b + 1 - a) 9
    where prve = if totalCifSucet a == 5 then a else a + cifSucet (10 - (totalCifSucet a) + 4)
--jCislaPocet a b = toInteger (length [x | x <- [a..b], totalCifSucet x == 5])

cifSucet :: Integer -> Integer   
cifSucet 0 = 0
cifSucet n = mod n 10 + cifSucet (div n 10)

totalCifSucet :: Integer -> Integer
totalCifSucet n |n < 10 = n
                |otherwise = totalCifSucet (cifSucet n)