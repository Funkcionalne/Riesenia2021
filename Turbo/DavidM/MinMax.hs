module MinMax where

data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)

minmax :: (Ord t) => (BTree t) -> (t, t)
minmax (Node Nil x Nil) = (x, x)
minmax (Node  l  x Nil) = (min a x, max b x) where (a, b) = minmax l
minmax (Node Nil x  r ) = (min a x, max b x) where (a, b) = minmax r
minmax (Node  l  x  r ) = (minimum [la, x, ra], maximum [lb, x, rb])
    where (la, lb) = minmax l
          (ra, rb) = minmax r

-- main = print $ minmax (Node (Node Nil 3 (Node Nil 1 Nil)) 4 (Node Nil 5 Nil))
