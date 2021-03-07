module Babylon
where 
import qualified Data.Set as Set

{-
najmensie :: Int -> [Integer]
najmensie n= take n $ Set.toAscList $ pom (Set.fromList [1]) (Set.fromList [1]) (n)

pom known new n = if (Set.size known  >=n) && ((Set.elemAt n known )<(Set.elemAt 0 new)) then known
		   else pom (Set.union known ssF)  ssF n
		   where 
		   s2 = Set.map (*2) new
		   s3 = Set.map (*3) new
		   s5 = Set.map (*5) new
		   ss = Set.union s2  (Set.union s3 s5)
		   ssF =Set.filter (\x->Set.notMember x  known) ss
		   
spravne :: Int -> Integer
spravne n = last $ najmensie n
-}


najmensie :: Int -> [Integer]
najmensie n= take n hamming 

spravne :: Int -> Integer
spravne n = head $ drop (n-1) hamming


-- v istom bode treba prejst z logiky do politiky a pouzit co je :(
-- https://rosettacode.org/wiki/Hamming_numbers?fbclid=IwAR3uLxgewi02OHwDKDMEg6uUV3d_5S_Bxy4zTPMChaFKhEVZzdsqPgGZhyc#Haskell
hamming = 1 : map (2*) hamming `union` map (3*) hamming `union` map (5*) hamming
 
union a@(x:xs) b@(y:ys) = case compare x y of
            LT -> x : union  xs  b
            EQ -> x : union  xs  ys
            GT -> y : union  a   ys
--
-- PS nice trick s nekonecnymi listami, ale C like styl mam stale radsej :)