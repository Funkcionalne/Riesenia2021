module Stredo where

stredoCislo :: Integer -> Bool
stredoCislo x = kontrola (show x)
stredoSlovo :: String -> Bool
stredoSlovo x= kontrola x


kontrola :: String -> Bool
kontrola xs |length xs == 0 = True 
			|length xs == 1 = elem (xs !! 0) ['0','1','8','I','N','H','O','S','X','Z']
			|otherwise = if ((prvy == '0') && (posledny == '0')) || ((prvy == '1') && (posledny == '1')) || ((prvy == '8') && (posledny == '8')) then kontrola (tail(init xs))
					else if ((prvy == '6') && (posledny == '9')) || ((prvy == '9') && (posledny == '6')) then kontrola (tail(init xs))
					else if ((prvy == 'W') && (posledny == 'M')) || ((prvy == 'M') && (posledny == 'W')) then kontrola (tail(init xs))
					else if ((prvy == 'I') && (posledny == 'I')) || ((prvy == 'N') && (posledny == 'N')) || ((prvy == 'H') && (posledny == 'H')) || ((prvy == 'O') && (posledny == 'O')) || ((prvy == 'S') && (posledny == 'S'))  || ((prvy == 'Z') && (posledny == 'Z'))  || ((prvy == 'X') && (posledny == 'X')) then kontrola (tail(init xs))
					else False
					where
						prvy = head xs 
						posledny = last xs
