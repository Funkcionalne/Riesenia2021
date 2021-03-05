module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce m = [ [ m !! (3*sy+y) !! (3*sx+x) | y<-t, x<-t ] | sy<-t, sx<-t ]
    where t = [0,1,2]

main = print $ sudokuStvorce [
    [1,7,5,3,2,8,4,9,6],
    [9,4,2,6,7,1,3,8,5],
    [3,6,8,5,9,4,2,7,1],
    [8,2,9,1,3,5,6,4,7],
    [6,5,3,4,8,7,9,1,2],
    [7,1,4,9,6,2,5,3,8],
    [2,3,1,8,4,6,7,5,9],
    [4,8,7,2,5,9,1,6,3],
    [5,9,6,7,1,3,8,2,4]]
