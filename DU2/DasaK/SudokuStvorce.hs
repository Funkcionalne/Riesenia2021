module SudokuStvorce where

import Data.List

-- kontrola ci v zozname sa nachadza 9 cisel od 1 po 9
check :: [Int] -> Bool
check xs = 9 == (length $ nub xs)

-- postupne kontrolujem najprv ci riadky su v poriadku, potom stlpce,
-- a nakoniec mensie 3x3 stvorce
testSudokuStvorce :: [[Int]] -> Bool
testSudokuStvorce xs = (and [check $ xs!!i | i<-[0..8]]) &&
                       (and [check [xs!!j!!i | j<-[0..8]] | i <-[0..8]]) &&
                       (and [check $ concat [x, y, z] | i<-[0,3,6], b<-[3,6,9],
                       let a = b - 3,
                       let x = drop a (take b $ xs!!i), 
                       let y = drop a (take b $ xs!!(i+1)),
                       let z = drop a (take b $ xs!!(i+2))])


--x = [[1,2,3,4,5,6,7,8,9],[4,5,6,7,8,9,1,2,3],[7,8,9,1,2,3,4,5,6],[2,1,4,3,6,5,8,9,7],[3,6,5,8,9,7,2,1,4],[8,9,7,2,1,4,3,6,5],[5,3,1,6,4,2,9,7,8],[6,4,2,9,7,8,5,3,1],[9,7,8,5,3,1,6,4,2]]