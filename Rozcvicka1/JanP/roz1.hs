-- v trinastkovej sustave

pocetCifierFact n = floor (1 + (sum $ map (logBase 13) [1..n]))
-- pocetCifierFact 1000 == 2305

pocetKoncovychNulFact n = sum $ takeWhile (>0) $ iterate (`div` 13) (div n 13)
-- pocetKoncovychNulFact 1000 == 81

pocetNul n  | n == 0 = 1
            | n < 13 = 0
            | mod n 13 == 0 = 1 + pocetNul (div n 13)
            | otherwise = pocetNul (div n 13)

pocetNulFact n = pocetNul $ product [1..n]
-- pocetNulFact 1000 == 237
