module Magic11 where

kontrapriklad :: Int
kontrapriklad = if xs == [] then 0 else xs!!0
    where xs = [x | x <- obdlznikove, delitelne11 x == False]
	
obdlznikove :: [Int]
obdlznikove = [(10^3)*(xs!!i!!j)+((10^2)*(xs!!x!!j))+((10)*(xs!!x!!y))+(xs!!i!!y) | i <- [0..2], j<- [0..2], x <- [0..2], y <- [0..2]] 
    where xs = [[1, 2, 3],[4, 5, 6],[7, 8, 9]]

delitelne11    :: Int -> Bool
delitelne11 n 
   | n == 0   = True
   | n < 11   = False
   |otherwise = delitelne11 (abs(e-o)) 
   where e = sum [x | (x,i) <- (zip (cifry n) [0..]) , even i]
         o = sum [y | (y,i) <- (zip (cifry n) [0..]) , odd i]

cifry     :: Int -> [Int]    
cifry 0 = []
cifry n = cifry (div n 10) ++ [mod n 10]  