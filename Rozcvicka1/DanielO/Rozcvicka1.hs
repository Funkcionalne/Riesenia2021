module Rozcvicka1 where

pocetBinCifierFakt :: Int -> Int
pocetBinCifierFakt n = ceiling $ sum $ map (logBase 2 . fromIntegral) [1..n]

pocetBinKoncovychNul :: Int -> Int
pocetBinKoncovychNul n | (==0)(n `mod` 2) = 1 + pocetBinKoncovychNul (n `div` 2)
                       | otherwise = 0

pocetBinKoncovychNulFakt :: Int -> Int
pocetBinKoncovychNulFakt n = sum $ map pocetBinKoncovychNul [1..n]

celkovoBinNul :: Integer -> Integer
celkovoBinNul 0 = 0
celkovoBinNul n = (1 - (n `mod` 2)) + celkovoBinNul (n `div` 2)

celkovoBinNulFakt :: Integer -> Integer
celkovoBinNulFakt n = celkovoBinNul (product [1..n])

main = do
    print $ pocetBinCifierFakt 1000
    print $ pocetBinKoncovychNulFakt 1000
    print $ celkovoBinNulFakt 1000
