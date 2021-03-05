module Cifry where
cifry     :: Integer -> [Integer]
cifry = reverse . cifryR


cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)
