module Podmnoziny where

-- rekurzivne vytvaranie podmnoziny, zoznam skracujem o prvy prvok
podmnoziny :: [t] -> [[t]]
podmnoziny [] = [[]]
podmnoziny xs = [(head xs):x | x<- podmnoziny (tail xs)] ++ (podmnoziny (tail xs))


-- vraciam si binarne kody tak ze rozdiel 2 po sebe iducich kodov je len v jednom bite
-- bianrne kody generujem dlzky n
grayCode :: Int -> [[Int]]
grayCode 0 = [[0]]
grayCode 1 = [[0], [1]]
grayCode n = [ 0:w | w <- words ] ++ [ 1:w | w <- (reverse words)] 
                where words = grayCode (n-1)

-- dostanem binarny kod a zoznam, ponecham len tie indexy kde mam
-- v binarnom kode 1tku
translate :: [t] -> [Int] -> [t]
translate xs g = [xs!!i | i<-[0..length xs - 1], g!!i == 1]

-- vyuzijem grayCode a translate na vygenerovanie podmnozin v poradi
podmnozinyVPoradi :: [t] -> [[t]]
podmnozinyVPoradi xs = [translate xs g | g <- grayCode (length xs)]
