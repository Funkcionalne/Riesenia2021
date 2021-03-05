module Rok2020 where

dvojica :: [Integer] -> Integer
dvojica xs = if length ys == 0 then -1 else maximum ys
    where ys = [x*y | x <- xs, y <-xs, x+y == 2020, x /= y]

stvorica:: [Integer] -> Integer
stvorica xs = if length ys == 0 then -1 else maximum ys
    where ys = [(xs!!i)*(xs!!j)*(xs!!k)*(xs!!l) | i <- [0..(length xs)-4], j <- [i+1..(length xs)-3], k <- [j+1..(length xs)-2], l <- [k+1..(length xs)-1], (xs!!i)+(xs!!j)+(xs!!k)+(xs!!l) == 2020]