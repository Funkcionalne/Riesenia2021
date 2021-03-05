module Rok2020 where
import Data.List

dvojica :: [Integer] -> Integer
dvojica [] = -1
dvojica xs = maximum $ [(xs !! x)*(xs !! y) | x <- [0..(length xs -1)] , y <- [x+1..(length xs -1)], (xs !! x)+(xs !! y) == 2020] ++ [-1]

stvorica:: [Integer] -> Integer
stvorica [] = -1
stvorica xs = maximum $ [(najdene !! 0) * (najdene2 !! 0) | s <- [1..1010], let najdene = (dvojicaMod (sort xs) (-1) (-1) s), let najdene2 = ((dvojicaMod (sort xs) (najdene !! 1) (najdene !! 2) (2020 - s)) ), toInteger (najdene !! 0) > 0, toInteger (najdene2 !! 0)> 0 ] ++[-1]

dvojicaMod :: [Integer] -> Integer -> Integer -> Integer -> [Integer]
dvojicaMod [] a b n = [-1,10000,10000]
dvojicaMod [x] a b n = [-1,10000,10000]
dvojicaMod xs a b n =   if toInteger (length xs) == b+1 || toInteger (length xs) == a+1 then (dvojicaMod (init xs) a b n)
						else if a == 0 || b == 0 then rekHeadUpdate
						else if sucet < n then max [-1,10000,10000] rekHeadUpdate
						else if sucet > n then max [-1,10000,10000] rekTail
						else max [p1*p2,0,toInteger (length xs) -1] rekHeadUpdate
						where 
							p1 = head xs
							p2 = last xs
							sucet = p1 + p2
							rekHead = (dvojicaMod (tail xs) (a-1) (b-1) n)
							rekTail = (dvojicaMod (init xs) a b n) 
							rekHeadUpdate = [rekHead !! 0, rekHead !! 1 + 1, rekHead !! 2 + 1]
							