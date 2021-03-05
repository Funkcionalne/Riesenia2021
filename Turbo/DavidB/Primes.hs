module Primes where
primes :: [Integer]

delitele x = length [t| t<-[1..x],x `mod` t ==0]

primes = map (\x-> if delitele x ==2 then x else 0) [1..]