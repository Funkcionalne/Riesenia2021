module PaliStvorce where 

import Data.List

-- test ci je zoznam palindrom
palindrom :: [Integer] -> Bool
palindrom [] = True
palindrom xs | a /= b = False
             | otherwise = palindrom $ take n (drop 1 xs)
                where 
                    a = head xs
                    b = last xs
                    n = length xs - 2

-- prevod cisla na pole cifier
toList :: Integer -> [Integer]
toList 0 = []
toList n = (toList $ n `div` 10) ++ [n `mod` 10]

-- prevod pola cifier na cislo
toInt :: [Integer] -> Integer
toInt xs = sum [(xs!!i)*10^(length xs - 1 - i) | i<-[0..(length xs - 1)]]

-- overenie ci pole cifier tvori take cislo ce je perfect square
isSquare :: [Integer] -> Bool
isSquare xs = (round . sqrt $ fromIntegral n) ^ 2 == n
            where n = toInt xs

{-
-- prvy pokus generovania, dosiahla som len prve 3
generate :: Integer -> [[Integer]]
generate 0 = [[]]
generate n = [ a:x | a<-[0,2,3,4,6,7,8,9], x<-(generate (n-2))]

generateX :: Integer -> [[Integer]]
generateX n = [ a:x | a<-[1,4,5,6,9], x<-(generate (n-2))]

getNum :: [Integer]
getNum = map (\x -> toInt (x ++ (reverse x))) 
                $ filter (\x -> isSquare (x ++ (reverse x))) 
                    $ concat [generateX i | i<-[2,4..17]]

nte :: Int -> Integer
nte n | n > 3 = 0
      | otherwise = getNum!!(n-1)
                --(take (n+1) [a*a | a <-[10..], let z = toList (a*a),
            --(length z) `mod` 2 == 0, palindrom z])!!(n-1)

ok :: Integer -> Bool
ok n = (palindrom xs) && (length xs `mod` 2 == 0) where xs = toList n   
-} 

generate :: Integer -> [[Integer]]
generate 0 = [[]]
generate n = [ a:x | a<-[0..9], x<-(generate (n-2))]

-- druha/predposledna cifra je dana
getNextDigit :: Integer -> [Integer]
getNextDigit n | n == 1 || n == 4 || n == 9 = [0,2,4,6,8]
               | n == 5 = [5]
               | n == 6 = [1,3,5,7,9]
               | otherwise = []

generate' :: [[Integer]] -> Integer -> [[Integer]]
generate' xs n = [b++a | a <- (concat [[x:y | x<-getNextDigit (head y)] | y<-xs]),
                         b <- generate (n-2)]

-- generujeme cisla, mocnina moze zacinat len na  1, 4, 5, 6, 9
-- vyfiltrujeme mocniny a zo zoznamov cisel vytvorime cisla
getPalindromicSquare :: Integer -> Integer
getPalindromicSquare n = head $ map (\x -> toInt ((reverse x) ++ x)) 
                            $ filter (\x -> isSquare ((reverse x) ++ x)) 
                                $ concat [generate' [[i]]  (n-2) | i<-[1, 4, 5, 6, 9]]

-- chcem cisla co maju parnu dlzku + podla tohoto (http://oeis.org/A034822) 
-- vieme ze chceme take cislo ktore maju dlzku 6, 12, 16, 22, 26, 28, ...
palindromicSquares :: Int -> Integer
palindromicSquares n = getPalindromicSquare ([6, 12, 16, 22, 26, 28, 32]!!(n-1))

nte :: Int -> Integer
nte n = palindromicSquares n
   
-- http://oeis.org/A016113   
--1: (3) 836 -> 698896 (6) [6,9,8,8,9,6]
--2: (6) 798644 -> 637832238736 (12) [6,3,7,8,3,2,2,3,8,7,3,6]
--3: (8) 64030648 -> 4099923883299904 (16) [4,0,9,9,9,2,3,8,8,3,2,9,9,9,0,4]
--4:
--5:




