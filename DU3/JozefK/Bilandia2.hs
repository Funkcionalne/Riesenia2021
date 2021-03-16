module Bilandia2 where


dobryNakup :: Integer -> Bool
dobryNakup n | checkPower(n) = True            --ak je suma mocnina dvojky, nemáme žiaden výdavok, takže pohoda
             | checkPower(closest-n) = True      --ak nie, tak výdavok max 1 mince je vtedy, keď rozdiel najbližšej väčšej mocniny dvojky a sumy je mocnina 2
             | otherwise = False
               where logo = floor((logBase 2) (fromIntegral n))
                     closest = 2^(logo+1)
               
checkPower :: Integer -> Bool
checkPower n = res == fromIntegral (floor res)
              where res = (logBase 2) (fromIntegral n)