module Luhn where

sub9 :: Int -> Int
sub9 n = if n > 9 then n-9 else n

luhnsum :: Integer -> Int
luhnsum 0 = 0
luhnsum n = z + sub9 (2*y) + luhnsum (n `div` 100)
    where z = fromIntegral (n `mod` 10)
          y = fromIntegral (n `div` 10 `mod` 10)

cardnumber :: Integer -> Bool
cardnumber n = (luhnsum n) `mod` 10 == 0

main = print $ luhnsum 8115
