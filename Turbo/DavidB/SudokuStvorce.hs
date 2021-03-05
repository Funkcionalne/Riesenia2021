module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce xs = [[xs !! a !! b |a<-[x,x+1,x+2],b<-[y,y+1,y+2]]|x<-[0,3,6], y<-[0,3,6]]
