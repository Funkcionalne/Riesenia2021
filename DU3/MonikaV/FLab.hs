module FLab where

import Data.List
type  Lab = [[Int -> Int]]


findMaxPath          :: Lab -> Int -> Int
findMaxPath lab init = maximum [foldl (\i f -> (f i)) init funct | funct <- px] where px = filter ((== (length lab)*(length (head lab))) . length) (paths lab [] 0 0)

paths :: Lab -> [[Int]] -> Int -> Int -> [[Int -> Int]]
paths xxs visited r s |cond = (map (x:) (paths xxs vx (r+1) s)) ++ (map (x:) (paths xxs vx (r-1) s)) ++ (map (x:) (paths xxs vx r (s+1))) ++ (map (x:) (paths xxs vx r (s-1)))
                      |otherwise = [[]]
                      where {cond = (r < length xxs && s < length (head xxs) && r >= 0 && s >= 0 && (elem [r, s] visited) == False);
                             x = (xxs!!r!!s);
                             vx = [r, s]:visited;
                            }