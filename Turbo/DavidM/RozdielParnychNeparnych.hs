module RozdielParnychNeparnych where

rozdielParnychNeparnych :: [Integer] -> Integer
rozdielParnychNeparnych xs = abs $ foldl (\r x -> x-r) 0 xs

main = print $ rozdielParnychNeparnych [4,3,8,3,4,5,7,8,4]
