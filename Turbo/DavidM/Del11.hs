module Del11 where

cifernyRoz :: Integer -> Integer
cifernyRoz 0 = 0
cifernyRoz n = (n `mod` 10) - cifernyRoz (n `div` 10)

delitelne11 :: Integer -> Bool
delitelne11 n
    | n == 0 = True
    | n < 10 = False
    | otherwise = delitelne11 $ abs $ fromIntegral $ cifernyRoz n

main = print (delitelne11 121, delitelne11 123, delitelne11 132,
    delitelne11 3432323523452312349023472879128347102741029384239480129840293481203948120394812034986)
