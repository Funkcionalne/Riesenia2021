module Primes where

isprime :: Integer -> Bool
isprime n = null $ filter (\i -> n `mod` i == 0) $ takeWhile (\i -> i*i <= n) [2..]

primes :: [Integer]
primes = 0:[ if isprime n then n else 0 | n <- [2..] ]

main = print $ take 100 primes
