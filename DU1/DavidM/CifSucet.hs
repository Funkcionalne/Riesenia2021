module CifSucet where

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = (b + 1 - p) `div` 9
    where p = ((a + 3) `div` 9)*9 - 3  -- najblizsie cislo <=a so zvyskom 6 mod 9

main = print $ jCislaPocet 8 18
