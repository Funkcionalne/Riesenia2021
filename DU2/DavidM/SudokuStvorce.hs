module SudokuStvorce where

import Data.List

stvorce :: [[Int]] -> [[Int]]
stvorce m = [ [ m !! (3*sy+y) !! (3*sx+x) | y<-t, x<-t ] | sy<-t, sx<-t ] where t = [0,1,2]

testRiadok :: [Int] -> Bool
testRiadok xs = sort xs == [1..9]

testSudokuStvorce :: [[Int]] -> Bool
testSudokuStvorce s = all testRiadok (s ++ transpose s ++ stvorce s)
