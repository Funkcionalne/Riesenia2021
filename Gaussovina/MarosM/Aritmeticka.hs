module Aritmeticka where

aritmSucet :: Integer -> Integer -> Integer -> Integer
aritmSucet a d b = if d == 0
then a
else div (((div (b - a) d) + 1) * (a + b)) 2



