module Cifry where

cifryR :: Integer -> [Integer]
cifryR = map (fromIntegral . flip mod 10) . takeWhile (>0) . iterate (flip div 10)

cifry :: Integer -> [Integer]
cifry = reverse.cifryR
