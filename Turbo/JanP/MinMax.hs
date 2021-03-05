module MinMax where
data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)

minmax :: (Ord t) => (BTree t) -> (t, t)
minmax (Node Nil x Nil) = (x, x)
minmax (Node Nil x r) = (min x rmin , max x rmax)
    where (rmin,rmax) = minmax r
minmax (Node l x Nil) = (min lmin x , max lmax x)
    where (lmin,lmax) = minmax l
minmax (Node l x r) = (min (min lmin x) rmin , max (max lmax x) rmax )
    where (lmin,lmax) = minmax l
          (rmin,rmax) = minmax r
