module Aritmeticka where

aritmSucet :: Integer -> Integer -> Integer -> Integer
aritmSucet a 0 b = a
aritmSucet a d b = count (((b - a) `div` d) + 1) a d

count :: Integer -> Integer -> Integer -> Integer
count n a d = n*a + ((n*(n-1)) `div` 2)*d
