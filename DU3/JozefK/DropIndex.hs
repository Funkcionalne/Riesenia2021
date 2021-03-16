module DropIndex where

--nevedel som čo keď je zlý index, tak hodím error...error z :https://wiki.haskell.org/Handling_errors_in_Haskell

drop'       :: Int -> [t] -> [t]
drop' n xs  =  (foldr pom (\_ -> []) xs) n where  
                  pom x h n = if n <= 0 then x:(h (n-1))
                              else h (n-1)

(!!!!)      :: [t] -> Int -> t
xs !!!! n   =  (\(real, helpo) -> real) (foldl check (error "Index out of range (probably)",n) xs)
                where check (real, n) x | n == 0 = (x, n-1)
                                        | otherwise = (real, n-1)

