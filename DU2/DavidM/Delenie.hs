module Delenie where

splitWords :: (Char -> Bool) -> String -> [String]
splitWords _ [] = []
splitWords p s = word : (splitWords p $ dropWhile p rest)
    where (word, rest) = break p s
