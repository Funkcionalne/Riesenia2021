module Primes where

primes :: [Integer]
primes = [0] ++ map (\x -> if isPrime x then x else 0) [2..]

pow :: Integer -> Integer -> Integer -> Integer
pow a x m = if x == 0 then 1
    else let p = pow a (x `div` 2) m in
        if ((x `mod` 2) == 1) then ((p * p * a) `mod` m)
        else if ((((p * p) `mod` m)== 1) && (p /= 1) && (p /= (m - 1))) then 0
        else (p * p) `mod` m

isPrime :: Integer -> Bool
isPrime n = all (\a -> (a >= n) || pow a (n-1) n == 1) [2,3,5,7,11,13,17,19,23,29]
