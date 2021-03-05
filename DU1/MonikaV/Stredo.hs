module Stredo where 

stredo :: (Eq a) => [a] -> [[a]] -> Bool 
stredo xs vals | xs == [] = True
               | length xs == 1 = elem (head xs) (concat vals)
               | otherwise = elem [head xs, last xs] vals && if length xs < 3 then True else stredo (init (tail xs)) vals

stredoCislo :: Integer -> Bool
stredoCislo n = stredo (cifry n) [[0, 0], [1, 1], [6, 9], [8, 8], [9, 6]]
--stredoCislo n |length xs == 1 = elem (head xs) [0, 1, 6, 9]
--              |otherwise = elem [head xs, last xs] [[0, 0], [1, 1], [6, 9], [8, 8], [9, 6]] && if length xs < 3 then True else stredoCislo (cislo (init (tail xs)))
--	          where xs = cifry n
			        
stredoSlovo :: String -> Bool
stredoSlovo xs = stredo xs ["MW", "WM", "II", "OO", "SS", "ZZ", "HH",  "NN", "XX"]
--stredoSlovo xs | xs == [] = True
--               | length xs == 1 = elem (head xs) "WIOSHZMNX"
--               | otherwise = elem [head xs, last xs] ["MW", "WM", "II", "OO", "SS", "ZZ", "HH", "NN", "XX"] && if length xs < 3 then True else stredoSlovo (init (tail xs))
					
cifry :: Integer -> [Integer]    
cifry 0 = []
cifry n = cifry (div n 10) ++ [mod n 10]

--cislo :: [Integer] -> Integer
--cislo [] = 0
--cislo xs = 10^((length xs)-1) * (head xs) + cislo (tail xs)  

--NISIN
--NOON
--SWOOMS
--SWIMS
--NON
--SIS
--SOS