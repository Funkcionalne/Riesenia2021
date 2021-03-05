module CNN where

cifry     :: Integer -> [Integer]
cifry = reverse . cifryR

cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)

cifryBin     :: Integer -> [Integer]
cifryBin = reverse . cifryBinR

cifryBinR    :: Integer -> [Integer]
cifryBinR n
    | n == 0 = [0]
    | n < 2 = [n]
    | otherwise = [n `mod` 2] ++ cifryBinR (n `div` 2)

reverseNum :: Integer -> Integer
reverseNum n = foldr (\x -> \y -> x+10*y) 0 $ cifry n

pow :: Integer -> Integer -> Integer -> Integer
pow a x m = if x == 0 then 1
    else let p = pow a (x `div` 2) m in
        if ((x `mod` 2) == 1) then ((p * p * a) `mod` m)
        else if ((((p * p) `mod` m)== 1) && (p /= 1) && (p /= (m - 1))) then 0
        else (p * p) `mod` m

isPrime :: Integer -> Bool
isPrime n = all (\a -> (a >= n) || pow a (n-1) n == 1) [2,3,5,7,11,13,17,19,23,29]

isSheldon :: Integer -> Bool
isSheldon n = isPrime n && cifryBin n == cifryBinR n

isChuck :: Integer -> Bool
isChuck n = isSheldon n && isSheldon (reverseNum n)

cnn :: Int
cnn = fromInteger $ head $ filter isChuck [10..]
