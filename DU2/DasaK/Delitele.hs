module Delitele where

import Data.List

-- 2 cisla su nesuddelitelne ak ich spolocny delitel je iba 1tka
-- skontrolujem ci zoznamy delitelov cisel su disjunktne
-- do kazdeho zoznamu delitelov pridam este samotne cislo, kedze
-- nasa funkcia vracia len vlastne delitele
nesudelitelne  :: Int -> Int -> Bool
nesudelitelne a b = disjunktne (delitele a ++ [a]) (delitele b ++ [b])


-- kontrolujem ci 2 usporiadane zoznamy su disjunktne
-- porovnam ich prve prvky a podla toho spravim dalsi krok, ak sa rovnaju
-- tak zoznamy nie su disjunktne, inak skratim jeden zoznam o mensi prvok z 2
disjunktne :: [Int] -> [Int] -> Bool
disjunktne xs [] = True
disjunktne [] xs = True
disjunktne xs ys | a == b = False
                 | a < b = disjunktne (tail xs) ys
                 | otherwise = disjunktne xs (tail ys)
                 where a = head xs
                       b = head ys


-- cislo je prvocislo ak nema ziadneho vlastneho delitela 
prvocislo :: Int -> Bool
prvocislo 1 = False
prvocislo n = null (delitele n)


-- delitela cisla n usporiadane vzostupne
delitele :: Int -> [Int]
delitele n = [ a | a <- [2 .. (n-1)], n `mod` a == 0]
--delitele n = sort $ nub $ concat [ [a, b] | a<-[2..(floor $ sqrt $fromIntegral n)], n `mod` a == 0, let b = n `div` a]
