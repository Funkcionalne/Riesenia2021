module RozdielParnychNeparnych where

rozdielParnychNeparnych    :: [Integer] -> Integer
rozdielParnychNeparnych l = abs x where x = foldr (-) 0 l
