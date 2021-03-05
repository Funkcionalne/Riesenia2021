module Del11 where


intlist :: Integer -> [Integer]
intlist n | n<10 = [n]
          | otherwise = intlist (n `div` 10) ++ [n `mod` 10]
sucet [] a b = delitelne11 (abs (a-b))
sucet pole a b = sucet (tail pole) (b+head pole) a

delitelne11 :: Integer -> Bool
delitelne11 x | x==0 = True
              | x<10 = False
              | otherwise = sucet (intlist x) 0 0