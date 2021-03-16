module FLab where
type  Lab = [[Int -> Int]]

findMaxPath          :: Lab -> Int -> Int
findMaxPath lab init = findAllPaths lab [] 0 0 init

findAllPaths :: Lab -> [(Int,Int)] -> Int -> Int -> Int -> Int
findAllPaths lab visited i j vaha=  if length visited == (length lab) * (length (lab!!0)) then vaha
									else maximum ((minBound :: Int):maximalna)
							        where
										nova_vaha = (lab!!i!!j) vaha
										maximalna = [findAllPaths lab ((i,j):visited) (i+a) (j+b) nova_vaha | (a,b) <- [(0,1),(1,0),(-1,0),(0,-1)], (i+a) >= 0 , (j+b) >= 0, (i+a) < (length lab) , (j+b) < (length (lab!!0)), elem (i,j) visited == False]
										
			{--							
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
--}