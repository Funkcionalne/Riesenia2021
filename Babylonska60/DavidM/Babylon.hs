module Babylon where

import Data.List

-- 1. poduloha

podel :: Integer -> Integer -> Integer
podel b n = head $ dropWhile (\i -> i `mod` b == 0) $ iterate (`div` b) n

jeSpravne :: Integer -> Bool
jeSpravne n = (podel 2 $ podel 3 $ podel 5 n) == 1

najmensie :: Int -> [Integer]
najmensie n = take n $ filter jeSpravne [1..]

-- ----------------------------------------------------------------------------

-- 2. poduloha

{-
Uvazoval som nasledovne.
Nech k = 2^a * 3^b * 5^c. Inak povedane, log2(k) = a+b*log2(3)+c*log2(5).
Cize ak mame usporiadane "spravne" cisla podla velkosti, je to to iste, ako ked ich usporiadavame
podla ich hodnoty a+b*log2(3)+c*log2(5). To je ale linerna kombinacia a, b, c, inymi slovami, ak sa
na spravne cisla pozerame ako na body v priestore, ktoreho osi predstavuju hodnoty a, b, c, potom
vsetky spravne cisla, ktore su mensie ako spravne cislo k, sa nachadzaju pod rovinou
a+b*log2(3)+c*log2(5) = k.
Toto pouzijeme na riesenie - najprv si zratame maximalne a. Pre dane a totiz vieme vyjadrit objem
ihlana pod rovinou a tento objem zodpoveda poctu spravnych cisel, cize ak mame najst n-te spravne
cislo, pod prislusnou rovinou musi byt (priblizne) n cisel, z coho vieme zratat maximalne a.
Obdobne zratame maximalne b, c (su v pevnom pomere s a) a v list comprehension vygenerujeme vsetky
cisla v ihlane. Tie by nam uz iba stacilo usporiadat podla a+b*log2(3)+c*log2(5) a zobrat n-te,
spravime vsak este malu optimalizaciu pre drobne (linearne) zrychlenie - vieme, ze n-te spravne
cislo sa nachadza velmi blizko roviny danej maximalnymi a, b, c, vacsinu bodov s malou hodnotou
a+b*log2(3)+c*log2(5) nam teda staci odfiltrovat a usporiadavat len body blizko pri hranicnej rovine.
-}

log23, log25 :: Double
log23 = logBase 2 3
log25 = logBase 2 5

log2 :: Int -> Int -> Int -> Double
log2 a b c = fromIntegral a + fromIntegral b * log23 + fromIntegral c * log25

maximalneA :: Int -> Double  -- zratame z objemu ihlana, ktory je ~n
maximalneA n = (6 * fromIntegral n * log23 * log25) ** (1/3)

-- vrati vsetky body pod rovinou (a okolo nej), na ktorej sa nachadza n-te spravne cislo
vsetkyPodRovinou :: Int -> [(Double, Int, Int, Int)]
vsetkyPodRovinou n = [ (log2 a b c, a, b, c) | a<-[0..maxA], b<-[0..boundB a], c<-[0..boundC a b] ]
    where
        maxA = ceiling (maximalneA n)
        maxB = ceiling (maximalneA n / log23)
        maxC = ceiling (maximalneA n / log25)  -- maxA, maxB, maxC su zaokruhlene nahor
        boundB a = maxB * (maxA - a) `div` maxA  -- rata maximalne b pre dane a (z rovnice roviny)
        boundC a b = maxC * (maxA*maxB - a*maxB - b*maxA) `div` (maxA*maxB)
                                                -- rata maximalne c pre dane a, b (z rovnice roviny)

spravne :: Int -> Integer
spravne n = let (_, a, b, c) = last $ take t $ sort priRovine in 2^a * 3^b * 5^c
    where
        vsetky = vsetkyPodRovinou n
        (m, _, _, _) = maximum vsetky
        priRovine = filter (\(x, _, _, _) -> x > m - 10) vsetky
            -- ratame s tym, ze n-te a maximalne zo vsetkych budu mat "vzdialenost" max. 10 od
            -- seba - je to rozumny bezpecny odhad, ten priestor pri rovine je celkom uzky
        t = n + length priRovine - length vsetky


main = print $ map (\n -> (n, spravne n)) $ take 6 $ iterate (*10) 30
