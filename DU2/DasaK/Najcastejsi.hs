module Najcastejsi where

import Data.List
import Data.Function 

-- vytvorim zoznam dvojic (x, c) kde x bude predstavovat prvok a c pocet kolko krat sa
-- prvok nachazda v zozname, potom zoznam usporiadam podla druheho prvku dvojice
-- najcastejsi je posledny, najzriedkavejsi prvy

 
--najcastejsi :: [Int] -> Int
najcastejsi :: (Ord t) => [t] -> t
najcastejsi xs = fst $ last (sortBy (compare `on` snd) zoz)
                where zoz = [(x, c) | x <- (nub xs), let c = length $ filter (==x) xs]

--najzriedkavejsi :: [Int] -> Int
najzriedkavejsi :: (Ord t) => [t] -> t
najzriedkavejsi xs = fst $ head (sortBy (compare `on` snd) zoz)
                where zoz = [(x, c) | x <- (nub xs), let c = length $ filter (==x) xs]


-- usporiadam zoznam pomocou funkcie, vypocitam si index stredu, upravim ho
-- podla dlzky zoznamu (par/nepar) a vratim median
--median :: [Int] -> Int
median :: (Integral t) => [t] -> t
median xs = (sort xs)!!i 
       where n = length xs  
             x = n `div` 2
             i = if n `mod` 2 == 0 then x - 1 else x

