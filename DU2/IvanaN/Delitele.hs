module Delitele where
import Data.List

delitele :: Int -> [Int]
delitele n = [d | d <- [2..n-1], (mod n d) == 0]

prvocislo :: Int -> Bool
prvocislo 0 = False
prvocislo 1 = False
prvocislo n = null (delitele n)

disjunktne :: [Int] -> [Int] -> Bool
disjunktne xs ys = length (intersect xs ys) == 0

nesudelitelne :: Int -> Int -> Bool
nesudelitelne x y = 0 == length (intersect (del x) (del y)) where
            del n = [d | d <- [2..n], (mod n d) == 0]
