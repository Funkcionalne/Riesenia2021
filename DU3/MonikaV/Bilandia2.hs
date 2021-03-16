module Bilandia2 where

dobryNakup :: Integer -> Bool
dobryNakup n = bool where (_, bool) = foldl (\(c, b) x -> if x == 1 && c == 0 then (c, False) else (x, b)) (1, True) (toBin n)

toBin :: Integer -> [Integer]
toBin 0 = [0]
toBin n = if (div n 2) == 0 then [mod n 2] else toBin (div n 2) ++ [mod n 2]
