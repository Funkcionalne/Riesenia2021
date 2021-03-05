module RozdielParnychNeparnych where

rozdielParnychNeparnych    :: [Integer] -> Integer

rozdielParnychNeparnych xs = abs(uncurry (-) d) where d = foldr (\x y -> (snd y+x,fst y)) (0,0) xs