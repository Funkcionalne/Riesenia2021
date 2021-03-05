module Magic11 where

-- Stvoruholnik je obdlznik prave vtedy, ak kazde dve susedne strany su kolme a kazde dve protilahle
-- strany su rovnako dlhe. Vyskusame vsetky mozne stvorice klaves, pricom kolmost overime klasicky
-- skalarnym sucinom a dlzky Pytagorovou vetou. Vsimnime si, ze tento postup spravne funguje aj na
-- "degenerovane" stvrouholniky (take, kde niektore vrcholy splyvaju) - nulove dlzky naozaj
-- vychadzaju 0 a ak dva body splyvaju, nimi dana usecka vyjde kolma na akukolvek usecku.

x :: Int -> Int
x n = (n - 1) `mod` 3

y :: Int -> Int
y n = (n - 1) `div` 3

-- ci su spojnice a-b a b-c kolme
suKolme :: Int -> Int -> Int -> Bool
suKolme a b c = (x a - x b) * (x b - x c) + (y a - y b) * (y b - y c) == 0

-- ci su usecky a-b a c-d rovnako dlhe
suRovnakoDlhe :: Int -> Int -> Int -> Int -> Bool
suRovnakoDlhe a b c d = (x a - x b)^2 + (y a - y b)^2 == (x c - x d)^2 + (y c - y d)^2

jeObdlznik :: Int -> Int -> Int -> Int -> Bool
jeObdlznik a b c d = (suKolme a b c) && (suKolme b c d) && (suKolme c d a) && (suKolme d a b) &&
                     (suRovnakoDlhe a b c d) && (suRovnakoDlhe b c a d)

obdlzniky :: [(Int, Int, Int, Int)]
obdlzniky = [ (a,b,c,d) | a<-[1..9], b<-[1..9], c<-[1..9], d<-[1..9], jeObdlznik a b c d ]

kontrapriklady :: [Int]
kontrapriklady = [ n | (a,b,c,d) <- obdlzniky, let n = 1000*a+100*b+10*c+d, n `mod` 11 /= 0 ]

kontrapriklad :: Int
kontrapriklad = head (kontrapriklady ++ [0])

main = print $ kontrapriklad
