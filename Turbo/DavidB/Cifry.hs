module Cifry where

intlist :: Integer -> [Integer]
intlist n | n<10 = [n]
          | otherwise = intlist (n `div` 10) ++ [n `mod` 10]

cifry     :: Integer -> [Integer]
cifry = intlist
cifryR    :: Integer -> [Integer]            
cifryR = reverse . intlist