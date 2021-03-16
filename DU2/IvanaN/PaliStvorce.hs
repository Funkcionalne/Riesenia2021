module PaliStvorce where
import Data.List ()

nte :: Int -> Integer 
nte n = fromIntegral(calculate!!(n-1)) where
        calculate = take n $ [ n*n | n <- [100..], parnyPocet(n*n), isPalindrome (n*n)]

isPalindrome :: Integer -> Bool
isPalindrome n = show n == reverse(show n)

parnyPocet :: Integer -> Bool 
parnyPocet n = (length $ show n) `mod` 2 == 0  

