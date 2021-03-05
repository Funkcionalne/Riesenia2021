module CNN where

ispal :: [Char] -> Bool
ispal s = s == reverse s

tobin :: Int -> [Char]
tobin 0 = "0"
tobin 1 = "1"
tobin n = (tobin (n `div` 2)) ++ (tobin (n `mod` 2))

isprime :: Int -> Bool
isprime n = null $ filter (\i -> n `mod` i == 0) $ takeWhile (\i -> i*i <= n) [2..]

cnn :: Int
cnn = head [n | n <- [10..], ispal (show n), ispal (tobin n), isprime n]

main = print $ cnn
