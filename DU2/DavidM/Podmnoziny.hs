module Podmnoziny where

podmnoziny :: [t] -> [[t]]
podmnoziny [] = [[]]
podmnoziny (x:xs) = pxs ++ map (x:) pxs
    where pxs = podmnoziny xs

podmnozinyVPoradi :: [t] -> [[t]]
podmnozinyVPoradi [] = [[]]
podmnozinyVPoradi (x:xs) = (reverse pxs) ++ map (x:) pxs
    where pxs = podmnozinyVPoradi xs
