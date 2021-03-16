module SudokuStvorce where
import Data.List

testSudokuStvorce :: [[Int]] -> Bool 
testSudokuStvorce xs = validRows xs && validRows(transpose xs)

validRow :: [Int] -> Bool 
validRow xs = sort xs == [1,2,3,4,5,6,7,8,9]

validRows :: [[Int]] -> Bool 
validRows xs = length [x | x <- xs, validRow(x)] == 9


sudoku :: [[Int]]
sudoku = [
            [1,2,3,4,5,6,7,8,9],
            [4,5,6,7,8,9,1,2,3],
            [7,8,9,1,2,3,4,5,6],
            [2,1,4,3,6,5,8,9,7],
            [3,6,5,8,9,7,2,1,4],
            [8,9,7,2,1,4,3,6,5],
            [5,3,1,6,4,2,9,7,8],
            [6,4,2,9,7,8,5,3,1],
            [9,7,8,5,3,1,6,4,2]
        ]