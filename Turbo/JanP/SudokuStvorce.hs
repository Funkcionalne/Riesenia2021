module SudokuStvorce where

sudokuStvorce :: [[Int]] -> [[Int]]
sudokuStvorce sud = [[sud !! (bigx+x) !! (bigy+y) | x <- [0..2], y <- [0..2]] | bigx <- [0,3..6], bigy <- [0,3..6]]
