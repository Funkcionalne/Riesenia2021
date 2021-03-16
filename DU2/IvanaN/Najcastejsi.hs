module Najcastejsi where
import Data.List
import Data.Ord

najcastejsi :: [Int] -> Int
najcastejsi [] = 0
najcastejsi xs = head $ maximumBy (comparing length) $ group $ sort xs

najzriedkavejsi :: [Int] -> Int
najzriedkavejsi [] = 0
najzriedkavejsi xs = head $ minimumBy (comparing length) $ group $ sort xs


median :: [Int] -> Int
median [] = 0
median xs = (sort xs) !! ((length xs) `div` 2)