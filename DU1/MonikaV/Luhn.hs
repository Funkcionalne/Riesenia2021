module Luhn where
cardnumber :: Integer -> Bool

cardnumber n = mod (sum [if mod i 2 == 1 then totalCifSucet ((xs!!i)*2) else (xs!!i)| i <- [0..((length xs)-1)]]) 10 == 0
    where xs = cifry n

cifry :: Integer -> [Integer]    
cifry 0 = []
cifry n = cifry (div n 10) ++ [mod n 10]

cifSucet :: Integer -> Integer   
cifSucet 0 = 0
cifSucet n = mod n 10 + cifSucet (div n 10)

totalCifSucet :: Integer -> Integer
totalCifSucet n |n < 10 = n
                |otherwise = totalCifSucet (cifSucet n)