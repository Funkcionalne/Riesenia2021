module Postupnosti where
nsrp   :: (Ord t) => [t] -> Int
nsrp  [] = -1
nsrp  (x:xs) = (\(s,p,m) -> if m > s then m else s) $ foldl (\(s,p,m) -> \y -> if p < y then (s+1,y,m) else (1,y,max s m)) (1,x,1) xs

