module CNN where
cnn :: Int
cnn = head xs where xs = take 1 najdi

najdi :: [Int]
najdi = [x | x <-[11..], jePrvo x, jePrvo (otoc x), jePalindrom (toBin x), jePalindrom (toBin (otoc x))] 

jePrvo :: Int -> Bool
jePrvo n = n>1 && null delitele where 
        delitele = [d | d <- [2..(div n 2)], n `mod` d == 0]

cifry     :: Int -> [Int]    
cifry 0 = []
cifry n = cifry (div n 10) ++ [mod n 10]  

cislo :: [Int] -> Int
cislo [] = 0
cislo xs = 10^((length xs)-1) * (head xs) + cislo (tail xs) 

otoc :: Int -> Int
otoc n = cislo (reverse (cifry n))

jePalindrom :: [Int] -> Bool
jePalindrom xs |length xs == 1 = True
               |otherwise = head xs == last xs && if length xs < 3 then True else jePalindrom (init (tail xs))
			  
toBin :: Int -> [Int]
toBin 0 = [0]
toBin n = if (div n 2) == 0 then [mod n 2] else toBin (div n 2) ++ [mod n 2]