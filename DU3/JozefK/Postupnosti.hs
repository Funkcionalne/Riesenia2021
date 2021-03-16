module Postupnosti where


nsrp   :: (Ord t) => [t] -> Int  --podobný princíp riešenia ako pri !!!! v dropIndex
nsrp  [] = -1
nsrp  (x:xs) = (\(value, curo, maxo) -> maxo) (foldl check (x, 1, 1) xs)
                where check (lasto, curo, maxo) x | x <= lasto = (x, 1, maxo)
                                                  | curo >= maxo = (x, curo+1, curo+1)
                                                  | otherwise = (x, curo+1, maxo)