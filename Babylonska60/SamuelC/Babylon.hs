module Babylon where

merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y     = x : merge xs (y:ys)
                    | x > y     = y : merge (x:xs) ys
                    | otherwise = x : merge xs ys

gen = 1 : foldl merge [] (map (\x -> map (*x) gen) [2, 3, 5])

najmensie = flip take gen
spravne n = gen !! (n-1)
