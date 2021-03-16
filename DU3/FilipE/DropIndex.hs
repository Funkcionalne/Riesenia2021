module DropIndex where

drop'       :: Int -> [t] -> [t]
drop' n xs  =  (\(i,p) -> p) $ foldl (\(i,p) -> \x -> if i < n then (i+1,p) else (i+1,p++[x])) (0,[]) xs

(!!!!)      :: [t] -> Int -> t
xs !!!! n   =  (\(i,p) -> p) $ foldl (\(i,p) -> \x -> if i == n then (i+1,x) else (i+1,p)) (0,head xs) xs