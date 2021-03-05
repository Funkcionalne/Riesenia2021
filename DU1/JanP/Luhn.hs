module Luhn where

cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)

cardnumber :: Integer -> Bool
cardnumber n = checksum `mod` 10 == 0
    where checksum = sum [
                f x |
                (x,f) <- zip (cifryR n) $ cycle [id,(
                    \x -> if 2*x >= 10 then 2*x-9 else 2*x
                )]
            ]
