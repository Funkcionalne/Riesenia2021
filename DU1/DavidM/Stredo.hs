module Stredo where

dvojice :: [(Char, Char)]
dvojice = [('0','0'), ('1','1'), ('6','9'), ('8','8'), ('9','6'),
           ('H','H'), ('I','I'), ('M','W'), ('N','N'), ('O','O'),
           ('S','S'), ('W','M'), ('X','X'), ('Z','Z')]

zhodne :: Char -> Char -> Bool
zhodne a b = elem (a,b) dvojice

stredoCislo :: Integer -> Bool
stredoCislo n = stredoSlovo $ show n

stredoSlovo :: String -> Bool
stredoSlovo [] = True
stredoSlovo (x:[]) = zhodne x x
stredoSlovo (x:y:[]) = zhodne x y
stredoSlovo s = zhodne (head s) (last s) && (stredoSlovo $ init $ tail s)

main = print $ stredoCislo 6661999

{-
Ad Bonus: Tu je zopár slov čo som našiel.
Ak mám iba jeden pokus na submit, tak vyberám latinské SINIS (ak je dovolené aj skloňovanie),
resp. estónske/fínske NIIN (ak je dovolený iba základný tvar).

v základnom tvare:
INNI (islandčina)   - vnútri    https://en.wiktionary.org/wiki/inni
INNI (maďarčina)    - piť       https://en.wiktionary.org/wiki/inni
NIIN (estónčina)    - potom     https://en.wiktionary.org/wiki/niin
NIIN (fínčina)      - preto     https://en.wiktionary.org/wiki/niin
NOON (angličtina)   - poludnie  https://en.wiktionary.org/wiki/noon
OSSO (holandčina)   - dom       https://en.wiktionary.org/wiki/osso
OSSO (taliančina)   - kosť      https://en.wiktionary.org/wiki/osso
OSSO (portugalčina) - kosť      https://en.wiktionary.org/wiki/osso
SOOS (holandčina)   - ako       https://en.wiktionary.org/wiki/soos

vyskloňované/vyčasované:
ISSI  (taliančina)   - zdvíhaš                           https://en.wiktionary.org/wiki/issi
NININ (slovenčina)   - patriaci Nine                     https://en.wiktionary.org/wiki/Ninin
SINIS (latinčina)    - dopúšťaš, nechávaš                https://en.wiktionary.org/wiki/sinis
SONOS (esperanto)    - znejú                             https://en.wiktionary.org/wiki/sonos
SONOS (portugalčina) - množné číslo od spánok, ospalosť  https://en.wiktionary.org/wiki/sonos
-}
