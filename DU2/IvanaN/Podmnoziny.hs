module Podmnoziny where 
import Data.List

podmnoziny :: [t] -> [[t]]
podmnoziny [] = [[]]
podmnoziny (x:xs) =  map (x:) (podmnoziny xs) ++ podmnoziny xs 

podmnozinyVPoradi :: [t] -> [[t]]
podmnozinyVPoradi [] = [[]]
podmnozinyVPoradi (x:xs) =  map (x:) (podmnozinyVPoradi xs) ++ podmnozinyVPoradi xs 