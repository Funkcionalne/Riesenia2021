module Primes where

primes :: [Integer]
primes = map f [1..] where
    f 1 = 0
    f n | isprime n = n
        | otherwise = 0
    isprime x = not $ elem 0 $ map (mod x) (takeWhile (\z -> z*z<=x) [2..x])
