module Magic11 where

pad :: [[Int]]
pad = [
        [7, 8, 9],
        [4, 5, 6],
        [1, 2, 3]
    ]

kontrapriklad :: Int
kontrapriklad = head $ filter (\x -> x `mod` 11 /= 0) ( concat [
        [
            1000*(pad!!x1!!y1) + 100*(pad!!x2!!y1) + 10*(pad!!x2!!y2) + (pad!!x1!!y2),
            1000*(pad!!x1!!y1) + 100*(pad!!x1!!y2) + 10*(pad!!x2!!y2) + (pad!!x2!!y1)
        ]|
        x1 <- [0..2],
        x2 <- [0..2],
        y1 <- [0..2],
        y2 <- [0..2],
        x1 /= x2 && y1 /= y2
    ]) ++ [0]

