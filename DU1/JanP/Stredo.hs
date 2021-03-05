module Stredo where

cifry     :: Integer -> [Integer]
cifry = reverse . cifryR

cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)

rotate :: Char -> Char
rotate c = case c of
    '0' -> '0'
    '1' -> '1'
    '6' -> '9'
    '8' -> '8'
    '9' -> '6'
    'H' -> 'H'
    'I' -> 'I'
    'M' -> 'W'
    'N' -> 'N'
    'O' -> 'O'
    'S' -> 'S'
    'W' -> 'M'
    'X' -> 'X'
    'Z' -> 'Z'
    otherwise -> '\0'

stredoCislo :: Integer -> Bool
stredoCislo n = stredoSlovo $ show n

stredoSlovo :: String -> Bool
stredoSlovo s = s == (map rotate $ reverse s)
