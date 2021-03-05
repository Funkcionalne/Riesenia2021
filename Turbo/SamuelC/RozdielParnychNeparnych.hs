module RozdielParnychNeparnych where
rozdielParnychNeparnych    :: [Integer] -> Integer
rozdielParnychNeparnych x  = abs v where (_,v) = foldl (\(t,s) x -> (-t,s+t*x)) (1,0) x
