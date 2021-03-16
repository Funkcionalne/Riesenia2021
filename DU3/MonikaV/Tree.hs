module Tree where

--import Test.QuickCheck 

data BVS t = Nil | Node (BVS t) t (BVS t) deriving(Show, Ord, Eq)

-- toto neefektivne riesenie bolo na prednaske
isBVS    :: (Ord t) => BVS t -> Bool
isBVS Nil = True
isBVS (Node left value right) =
      (all (<value) (flat left))
      &&
      (all (>value) (flat right))
      &&
      isBVS left
      &&
      isBVS right

findBVS  :: (Ord t) => t -> (BVS t) -> Bool
findBVS _ Nil = False
findBVS x (Node left value right)  | x == value = True
                                | x < value = findBVS x left
                                | x > value = findBVS x right

flat  :: BVS t -> [t]                                
flat Nil = []
flat (Node left value right)  = flat left ++ [value] ++ flat right
-------------------------------- tu konci prednaska, zvysok doprogramujte:

-- maximalny prvok neprazdneho binarneho vyhladavacieho stromu, ak splna podmienku ...
maxBVS   :: (Ord t) => BVS t -> t
maxBVS (Node left value right) | right == Nil = value
                               | otherwise = maxBVS right

-- minimalny prvok neprazdneho binarneho vyhladavacieho stromu, ak splna podmienku ...
minBVS    :: (Ord t) => BVS t -> t
minBVS (Node left value right)| left == Nil = value
                              | otherwise = minBVS left

-- linearna verzia isBVS, strom prejde len raz
isBVSLinear    :: (Ord t) => BVS t -> Bool
isBVSLinear Nil = True
isBVSLinear bvs = sorted' (flat bvs) 

sorted' :: (Ord t) => [t] -> Bool
sorted' [] = True
sorted' [_] = True
sorted' (x1:x2:xs) = (x1 < x2) && (sorted' (x2:xs))

-- vlozenie, prvky v strome sa neopakuju
insertBVS      :: (Ord t) => t -> BVS t -> BVS t       
insertBVS x Nil = (Node Nil x Nil)
insertBVS x (Node left value right) | x < value = (Node (insertBVS x left) value right)
                                    | x > value = (Node left value (insertBVS x right))    
                                    | otherwise = (Node left value right)						

-- zmazanie
deleteBVS    :: (Ord t) => t -> BVS t -> BVS t
deleteBVS x Nil = Nil
deleteBVS x (Node Nil value Nil) = if x == value then Nil else (Node Nil value Nil)
deleteBVS x (Node left value Nil) = if x == value then left else if x < value then (Node (deleteBVS x left) value Nil) else (Node left value Nil) 
deleteBVS x (Node Nil value right) = if x == value then right else if x > value then (Node Nil value (deleteBVS x right)) else (Node Nil value right) 
deleteBVS x (Node left value right) | x == value = (Node left y (deleteBVS y right)) 
                                    | x < value = (Node (deleteBVS x left) value right)
									| otherwise = (Node left value (deleteBVS x right))
                                    where y = (minBVS right)

rekInsert :: Int -> Int -> BVS Int -> BVS Int
rekInsert k n bvs |k > n = bvs
                  |k == n = insertBVS k bvs
                  |otherwise = rekInsert (k+1) n (insertBVS k bvs)
									
qch1 :: (Ord t) => BVS t -> Bool 							
qch1 bvs = isBVS bvs == isBVSLinear bvs

qch2 :: (Ord t) => BVS t -> t -> Bool
qch2 bvs n = bvs == deleteBVS n (insertBVS n bvs)

qch3 :: (Ord t) => BVS t -> t -> Bool
qch3 bvs n = findBVS n (insertBVS n bvs)

qch4 :: (Ord t) => BVS t -> t -> Bool
qch4 bvs n = False == (findBVS n (deleteBVS n bvs))

qch5 :: (Ord t) => BVS t -> t -> Bool
qch5 bvs n = isBVS bvs == isBVS (insertBVS n bvs)

qch6 :: (Ord t) => BVS t -> t -> Bool
qch6 bvs n = isBVS bvs == isBVS (deleteBVS n bvs)

qch7 :: (Ord t) => BVS t -> t -> Bool
qch7 bvs n = findBVS n bvs == (elem n (flat bvs))

qch8 :: Int -> Bool
qch8 n = isBVS bvs where bvs = rekInsert 1 n Nil

qch9 :: Int -> Bool
qch9 n = length (flat bvs) == n where bvs = rekInsert 1 n Nil

