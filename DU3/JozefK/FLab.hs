module FLab where

import Data.List

type  Lab = [[Int -> Int]]

--neoptimálne pre filter, skúšam či prejde testami

getRoads :: Int -> Int -> [[Int]] -> [(Int, Int)] -> [[(Int, Int)]]
getRoads i j xxs toBeUsed | check i j xxs == False = [[]]
                          | (i,j) `elem` toBeUsed = [[]]
                          | otherwise = [(i,j):y | y<-getRoads (i+1) j xxs ((i, j):toBeUsed)] 
                         ++ [(i,j):y | y<-getRoads i (j+1) xxs ((i, j):toBeUsed)]
                         ++ [(i,j):y | y<-getRoads i (j-1) xxs ((i, j):toBeUsed)]
                         ++ [(i,j):y | y<-getRoads (i-1) j xxs ((i, j):toBeUsed)]
                          
check  :: Int -> Int -> [[Int]] -> Bool
check i j xxs = (i < (length xxs)) && (j < (length (xxs!!0))) && (i >= 0) && (j >= 0)   

roads :: Int -> [[(Int, Int)]] -> [[(Int, Int)]]
roads size xs = filter  (\x -> length x == size) xs 

                            
findMaxPath          :: Lab -> Int -> Int
findMaxPath lab init = maximum [getResult (tail x) lab init | x <- nub (roads size (getRoads 0 0 matrix []))]
                     where matrix = [[1 | x <- [1..length (lab!!0)]] | y <- [1..(length lab)]] 
                           size = (length (lab!!0)) * (length lab)
                           
getResult :: [(Int, Int)] -> Lab -> Int -> Int
getResult road lab init = foldl (\acc (i,j) -> (lab!!i!!j) . acc) (lab!!0!!0) road init

lab1  :: Lab
lab1  = [
          [ (+1), (+1),(+1) ],
          [ (+1), (+1),(+1) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab1 0 = 15

lab2  :: Lab
lab2  = [
          [ (+1), (+1),(+1) ],
          [ (+1), (+1),(*2) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab2 0 = 23

lab3  :: Lab
lab3  = [
          [ (+1), (+1),(+1) ],
          [ (*2), (+1),(*2) ],
          [ (+1), (*2),(+1) ]
        ]
-- findMaxPath lab3 0 = 31

lab4  :: Lab
lab4  = [
          [ (+1), (*2),(*3) ],
          [ (*2), (`div` 3),(*2) ],
          [ (+4), (*2),(+10) ]
        ]
-- findMaxPath lab4 0 = 108

lab5  :: Lab
lab5  = [
          [ (*2), (*3),(*5) ],
          [ (*8), (*9),(*4) ],
          [ (*7), (*6),(*10) ]
        ]

-- findMaxPath lab5 1 = 3628800