module CNN where

cnn :: Int
cnn = fromIntegral((filtruj 1000) !! 0  )

isPrime :: Integer -> Bool
isPrime n = length [d | d<-[2..floor(sqrt (fromIntegral n))], n `mod` d == 0] == 0

filtruj :: Integer -> [Integer]
filtruj n = [x | x <- [10..n], isPali (rozbi x) , isPrime (obratena (reverse (rozbi x))) , isPali (toBin x)]

toBin :: Integer -> [Integer]
toBin 0 = []
toBin n | n `mod` 2 == 1 = toBin (n `div` 2) ++ [1]
        | n `mod` 2 == 0 = toBin (n `div` 2) ++ [0]

isPali :: [Integer] -> Bool
isPali xs = xs == reverse xs 

rozbi :: Integer -> [Integer]
rozbi 0 = []
rozbi xs = rozbi (xs `div` 10) ++ [xs `mod` 10]

obratena :: [Integer] -> Integer
obratena [] = 0
obratena xs = if length xs == 1 then xs !! 0
				else (xs !! 0)*10^((length xs)-1) + obratena (tail xs)

 