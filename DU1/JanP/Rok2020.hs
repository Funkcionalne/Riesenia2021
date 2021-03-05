module Rok2020 where

dvojica :: [Integer] -> Integer
dvojica xs = foldr max (-1) [x1*x2 |
                            (i1,x1) <- zip [1..] xs,
                            (i2,x2) <- drop i1 $ zip [1..] xs,
                            x1+x2==2020]

stvorica:: [Integer] -> Integer
stvorica xs = foldr max (-1) [x1*x2*x3*x4 |
                            (i1,x1) <- zip [1..] xs,
                            (i2,x2) <- drop i1 $ zip [1..] xs,
                            (i3,x3) <- drop i2 $ zip [1..] xs,
                            (i4,x4) <- drop i3 $ zip [1..] xs,
                            x1+x2+x3+x4==2020]
