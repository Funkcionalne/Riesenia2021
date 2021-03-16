module Delenie where

-- zoberiem si podretazec retazca po prvy oddelovac a potom na zvysok retazca 
-- zavolam rekurzivne funckiu (splitWords), trivilany pripad - mame prazdy retazec
splitWords  :: (Char -> Bool) -> String -> [String]
splitWords fx "" = []
splitWords fx xs | fx $ head xs = (splitWords fx (tail xs))
                 | otherwise = 
                    (takeWhile (\z -> not $ fx z) xs) : (splitWords fx s2)
        where arry = [ i | i<-[0..(length xs - 1)], fx (xs!!i)]
              s2 = if length arry > 0 then drop (arry!!0) xs else []
    

-- testovacia funckia    
whiteSpace  :: Char -> Bool
whiteSpace ch = ch == ' '