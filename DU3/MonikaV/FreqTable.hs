module FreqTable where
import Data.List
import Data.Char

mostFrequent :: Int -> String -> String
mostFrequent n text = (init (concat [x++" " | x <- (reverse (take n (sorted (w : xs))))]))
   where (_, w, xs) = foldl (\(ch, w, xs) x -> if ch < 'A' then ((toLower x), [(toLower x)], xs) else if (toLower x) < 'a' then (x, "", w:xs) else ((toLower x), w++[(toLower x)], xs)) (' ', "", []) text

freqTable :: [String] -> [(Int, String)]
freqTable xs = reverse (sortOn fst (sortOn snd $ map (\x -> (length x, (head x))) $ group $ sort xs))

sorted :: [String] -> [String]
sorted xs = [x | (i, x) <- (freqTable xs)]