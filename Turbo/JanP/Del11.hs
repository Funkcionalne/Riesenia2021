module Del11 where
delitelne11    :: Integer -> Bool
delitelne11 n
    | n == 0 = True
    | n < 10 = False
    | otherwise = delitelne11 $ rozdielParnychNeparnych (cifryR n)

cifryR    :: Integer -> [Integer]
cifryR n
    | n == 0 = [0]
    | n < 10 = [n]
    | otherwise = [n `mod` 10] ++ cifryR (n `div` 10)

rozdielParnychNeparnych    :: [Integer] -> Integer
rozdielParnychNeparnych l = abs x where x = foldr (-) 0 l
