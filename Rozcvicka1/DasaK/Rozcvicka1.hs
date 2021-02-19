module Rozcvicka1 where

--na to aby sme nasli pocet cifier tak vyuzijeme vztah logaritmov (sucet-sucin),
--kedze chceme pocet cifier v 2kovej sustave tak pouzijeme logaritmus pri zaklade 2
pocetCifier2Sus :: Int -> Int
pocetCifier2Sus n = ceiling $ (sum $ (map (logBase 2) [1 .. fromIntegral n]))

--najdeme si najvacsie prvocislo v rozklade cisla, ktore berieme ako sustavu (teraz 2)
--a je to 2ka, potom najdeme pocet 2jek v prvociselnom rozklade cisla n
pocet2 :: Int -> Int
pocet2 n = length $ (takeWhile (\x -> x `mod` 2 == 0) $ iterate (`div` 2) n)

--pocet koncovych nul je sucet poctov 2ek v cislach 1..n
koncoveNuly2Sus :: Int -> Int
koncoveNuly2Sus n = sum $ (map pocet2 [1 .. n])

fact :: Integer -> Integer
fact n = product [1..n]

--na pocet vsetkych nul pouzijeme faktorial, ten nam vrati hodnotu v desiatkovej sustave,
--nemusime si pamatat presnu hodnotu v 2kovej sustave staci ked si budeme pamtat ci sa na pozicii
--nachadza 0/1 a postupne to spocitame, delime cislo 2kou, a dostaneme vysledok
pocetNul2Sus :: Integer -> Integer
pocetNul2Sus n = count (fact n)

count :: Integer -> Integer
count 0 = 0
count n =
    if (n `mod` 2 == 0) 
        then 1 + count (n `div` 2)
        else count (n `div` 2)

{-
"2kova sustava"
1000! ma v dvojkovom zapise 8530 cifier,
1000! ma 994 koncovych nul,
1000! ma 4742 vsetkych nul
-}

-- pre 12kovu sustavu postupujeme rovnako ako pri dvojkovej len pouzivame logaritmus pri
-- zaklade 12 a delime najvacsim prvocilsom v prvociselnom rozklade cisla 12,
-- pri pocte vsetkych nul budeme delit 12kou miesto 2, pri inych sustavach postupujeme podobne 
pocetCifier12Sus :: Int -> Int
pocetCifier12Sus n = ceiling $ (sum $ (map (logBase 12) [1 .. fromIntegral n]))

pocet3 :: Int -> Int
pocet3 n = length $ (takeWhile (\x -> x `mod` 3 == 0) $ iterate (`div` 3) n)

koncoveNuly12Sus :: Int -> Int
koncoveNuly12Sus n = sum $ (map pocet3 [1 .. n])

pocetNul12Sus :: Integer -> Integer
pocetNul12Sus n = count2 (fact n)

count2 :: Integer -> Integer
count2 0 = 0
count2 n =
    if (n `mod` 12 == 0) 
        then 1 + count2 (n `div` 12)
        else count2 (n `div` 12)

{-
"12kova sustava"
1000! ma v 12tkovom zapise 2380 cifier,
1000! ma 498 koncovych nul,
1000! ma 650 vsetkych nul
-}
