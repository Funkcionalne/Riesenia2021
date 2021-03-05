module Magic11 where

kontrapriklad :: Int
kontrapriklad = if length  pole == 0 then 0
				else (prvy !! 0)*1000 +  (prvy !! 1)*100 + (prvy !! 2)*10 + (prvy !! 3)
				where
					pole = [[x,x+b,x+a+b,x+a]| x <- [1..9], a <- [0,3,6], b <- [0,1,2], x+b < 10, x+a < 10, x+a+b<10, (mod (x+a) 3) == (mod x 3), (div x 3) == (div (x+b-1) 3), mod ((x+x+a+b)-(x+b+x+a)) 11 /= 0]
					prvy = head pole

-- Matematicky vieme, ze jedna o tri mozne pripady cisel. Prvy su cisla v tvare aaaa (kde a <- [1..9]), tu trivialne vidime ze ak scitame cifry na parnych poziciach a odcitame od suctu cifier na neparnych poziciach 
--(delitelnost 11) tak dostaneme (a+a) - (a+a) = 0 mod 11 = 0 takze cislo je delitelne 11
--Druhym pripadom je abba (kde a <- [1..9], b <- [1..9])a dostavame vzorec (a+b)-(b+a) = 0 - kedze scitavanie je komutativne
--Poslednym pripadom je abcd (kde a <- [1..9], b <- [1..9], c  <- [1..9],d <- [1..9]). Tu uz je pripadov viac (v prilohe pripady.png), ale greedy sposobom si ich vieme vsetky vypisat a vidime, ze pre vsetky plati a+c = b+d takze opat dostavame delitelne 11.
--Teda v pripade obdlznikovych cisel na cislach [1..9] neexistuje obdlznikove cislo, ktore nie je delitelne 11.