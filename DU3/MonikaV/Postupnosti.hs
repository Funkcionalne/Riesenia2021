module Postupnosti where
nsrp   :: (Ord t) => [t] -> Int
nsrp  [] = -1
nsrp  (x:xs) = max m n where (m, n, _) = foldl (\(m, n, y) z -> if y < z then (m, n+1, z) else if m < n then (n, 1, z) else (m, 1, z)) (1, 1, x) xs