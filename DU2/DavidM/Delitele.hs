module Delitele where

import Data.List

sqrti :: Int -> Int
sqrti = floor . sqrt . fromIntegral

delitele :: Int -> [Int]
delitele n = sort $ concat [ nub [i, n `div` i] | i <- [2..sqrti n], n `mod` i == 0 ]

prvocislo :: Int -> Bool
prvocislo n = n > 1 && (null $ delitele n)

disjunktne :: [Int] -> [Int] -> Bool
disjunktne xs ys = null $ intersect xs ys

nesudelitelne :: Int -> Int -> Bool
nesudelitelne a b = disjunktne (a : delitele a) (b : delitele b)

main = print $ nesudelitelne 3 12
