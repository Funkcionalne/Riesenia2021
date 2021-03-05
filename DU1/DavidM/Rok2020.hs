module Rok2020 where

import Data.List

suciny2 :: Integer -> [Integer] -> [Integer]
suciny2 n [] = []
suciny2 n [x] = []
suciny2 n xs
    | x + y == n = (x*y):(suciny2 n (tail $ init xs))
    | x + y <  n = suciny2 n (tail xs)
    | otherwise  = suciny2 n (init xs)
    where (x, y) = (head xs, last xs)

dvojica :: [Integer] -> Integer
dvojica xs = if null d then -1 else last d
    where d = suciny2 2020 (sort xs)

suciny4 :: [Integer] -> [Integer]
suciny4 xs = [ if null d then -1 else x*y*last d |
                (x:ys) <- tails xs,
                (y:zs) <- tails ys,
                2020-x-y > 0,
                let d = suciny2 (2020-x-y) zs ]

stvorica :: [Integer] -> Integer
stvorica xs = if null d then -1 else maximum d
    where d = suciny4 (sort xs)

main = do
    print $ dvojica  [1,2,4,5,6,2015,9,10,2010,3]
    print $ stvorica [1,2,4,5,6,2015,9,10,2010,3]
