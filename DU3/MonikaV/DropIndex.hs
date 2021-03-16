module DropIndex where

drop'       :: Int -> [t] -> [t]
drop' n xs  = ys where (_, ys) = foldr (\x (k, ys) -> if k > 0 then (k-1, x:ys) else (k, ys)) ((length xs)-n, []) xs

(!!!!)      :: [t] -> Int -> t
xs !!!! n   =  val where (val, _) = foldl (\(val, i) x -> if i == 0 then (x, -1) else (val, i-1)) ((head xs), n) xs  -- ak prazdny zoznam (head xs) padne, ak je n vacsie ako length xs vrati prvy prvok 