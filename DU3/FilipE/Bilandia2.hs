module Bilandia2 where
dobryNakup :: Integer -> Bool
dobryNakup x = if mocniny!!(length mocniny - 1)-x == 0 then True
			   else if elem (mocniny!!(length mocniny - 1)-x) mocniny then True
			   else False
				where 
					mocniny = najvyssiaMocnina x [1]

najvyssiaMocnina :: Integer -> [Integer] -> [Integer]
najvyssiaMocnina n x | n <= (x!!(length x - 1)) = x
					 | otherwise = najvyssiaMocnina n (x ++ [x!!(length x - 1)*2])