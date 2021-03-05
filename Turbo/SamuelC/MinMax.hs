module MinMax where

data BTree t = Node (BTree t) t (BTree t) | Nil deriving(Show, Eq)

tfold :: (Ord t) => (t -> t -> t) -> (BTree t) -> t
tfold f (Node Nil x Nil) = x
tfold f (Node Nil x rt)  = f x (tfold f rt)
tfold f (Node lt x Nil)  = f x (tfold f lt)
tfold f (Node lt x rt)   = foldl1 f [tfold f lt, x, tfold f rt]

minmax :: (Ord t) => (BTree t) -> (t, t)
minmax t = (tfold min t, tfold max t)
