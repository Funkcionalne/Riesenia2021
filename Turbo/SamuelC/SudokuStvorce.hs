module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce rows = [sq i j | i<-[0..2], j<-[0..2]] where
    sq oi oj = [rows !! (3*oi+i) !! (3*oj+j) | i<-[0..2], j<-[0..2]]
