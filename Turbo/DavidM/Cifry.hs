module Cifry where

cifryR :: Integer -> [Integer]
cifryR n
    | n < 10 = [n]
    | otherwise = (n `mod` 10):(cifryR (n `div` 10))

cifry :: Integer -> [Integer]
cifry n = reverse $ cifryR n

main = print $ cifry 918972645
