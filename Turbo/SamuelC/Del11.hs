module Del11 where

delitelne11 :: Integer -> Bool
delitelne11 n | n < 0  = delitelne11 (-n)
              | n == 0 = True
              | n < 10 = False
              | otherwise = del11rdigits $ rdigits n

rdigits :: Integer -> [Int]
rdigits = map (fromIntegral . flip mod 10) . takeWhile (>0) . iterate (flip div 10)

del11rdigits :: [Int] -> Bool
del11rdigits ds = delitelne11 (fromIntegral $ dsum ds)

dsum = sum . zipWith (*) (cycle [-1,1])
