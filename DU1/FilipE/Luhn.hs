module Luhn where
cardnumber :: Integer -> Bool
cardnumber n = (sum (map (\x -> if (x >= 10 ) then (x - 9) else x) (priprava(reverse (rozbi n))))) `mod` 10 == 0

rozbi :: Integer -> [Integer]
rozbi 0 = []
rozbi xs = rozbi (xs `div` 10) ++ [xs `mod` 10]

priprava :: [Integer] -> [Integer]
priprava [] = []
priprava xs = [2*(xs !! x) | x <- [0..(length xs -1)] ,  x `mod` 2 == 1] 
				++ [(xs !! x) | x <- [0..(length xs -1)] , x `mod` 2 == 0]
	
				