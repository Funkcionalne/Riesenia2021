module MinMax where

data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)

minmax :: (Ord t) => BTree t -> (t, t)
minmax (Node Nil x Nil) = (x, x)

minmax (Node Nil t y) = (minimum [fst b, t], maximum [snd b, t]) 
    where b = minmax y 

minmax (Node x t Nil) = (minimum [fst a, t], maximum [snd a, t]) 
    where a = minmax x

minmax (Node x t y) = (minimum [fst a, fst b, t], maximum [snd a, snd b, t]) 
    where a = minmax x
          b = minmax y 
