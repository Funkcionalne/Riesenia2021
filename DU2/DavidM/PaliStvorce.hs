module PaliStvorce where

{-
Jednoduchsi (pomalsi) postup: Skusame prirodzene i, kazde umocnime na druhu a pozrieme sa, ci je to
palindrom parnej dlzky. Staci nam pritom brat i delitelne 11-imi, lebo palindrom parnej dlzky zjavne
splna kriterium delitelnosti 11-imi, a teda aj jeho (celociselna) odmocnina musi byt delitelna
11-imi. Tento postup som pouzil pre i < 10^6, nech sa pri zlozitejsom posupe nemusime babrat s
malymi pripadmi.

Zlozitejsi (rychlejsi) postup: Nech hladany palindrom ma napriklad 14 cifier. Potom pri jednoduchsom
postupe by sme museli prehladavat 7-ciferne i. Namiesto toho budeme skusat len posledne 4 cifry i
(vo vseobecnosti - polovicu zaokruhlenu nahor). Pre kazdy 4-ciferny koniec i si zratame i^2 a vieme,
ze ak existuje vyhovujuce i s tymto koncom, prislusny palindrom ma rovnake posledne 4 cifry ako i^2.
Z toho vieme aj prve 4 cifry palindromu. Strednych 6 cifier skusime doplnit nulami resp. deviatkami,
tieto cisla odmocnime a dostali sme dolnu a hornu hranicu pre i. Z podstaty veci sa tieto cisla
zhoduju na vacsine prvych cifier, ked teda doplnime poslede 4 cifry, ktore pozname, dostaneme
niekolko (malo) skutocnych kandidatov na cele i pre dane koncove stvorcislie (toto rata funkcia
kandidati). A uz iba overime, ci je stvorec daneho kandidata skutocne palindrom parnej dlzky.
-}

jePal :: Integer -> Bool
jePal n = length s `mod` 2 == 0 && s == reverse s
    where s = show n

do6Cifier :: [Integer]
do6Cifier = filter jePal $ map (\i -> i*i) [11,22..999999]

kandidati :: Int -> Int -> Integer -> [Integer]
kandidati k h i = [ z*10^h+i | z <- [zackan (zaciatok*rad)..zackan ((zaciatok+1)*rad-1)] ]
    where zaciatok = read $ take h $ (reverse $ show $ i*i) ++ repeat '0'
          rad = 10^(2*k-h)
          zackan x = (floor $ sqrt $ fromIntegral x) `div` 10^h

kCifier :: Int -> [Integer]
kCifier k = filter jePal $ map (\i -> i*i) $ concat $ map (kandidati k h) [0..10^h-1]
    where h = (k+1) `div` 2

vsetky :: [Integer]
vsetky = do6Cifier ++ (concat $ map kCifier [7..])

nte :: Int -> Integer
nte n = last $ take n vsetky


main = print $ nte 1
