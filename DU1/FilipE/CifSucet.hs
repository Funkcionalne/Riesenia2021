module CifSucet where 

jCislaPocet :: Integer -> Integer -> Integer
jCislaPocet a b = toInteger(length [x | x <- [a..b], mod x 9 == 5])

-- Riesenie som napisal takymto LifeHackom, studoval som si Digital Root a zistil som, ze kedze vysledkom je vzdy cislo od 1-9, tak pre overenie ci sa Digit sum cisla rovna 5 staci aby bol zvysok po deleni 9 5.

