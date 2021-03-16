module Tree where
import Data.List
import Test.QuickCheck
import Control.Monad
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
minBVS (Node left value right) | left == Nil = value
							   | otherwise = minBVS left


-- linearna verzia isBVS, strom prejde len raz
isBVSLinear    :: (Ord t) => BVS t -> Bool
isBVSLinear Nil = True
isBVSLinear (Node Nil value Nil) = True		
isBVSLinear (Node Nil value right) = isBVSLinear right &&  minBVS right > value					  
isBVSLinear (Node left value Nil) = isBVSLinear left && maxBVS left < value 
isBVSLinear (Node left value right) = isBVSLinear left && isBVSLinear right && maxBVS left < value &&  minBVS right > value 
									  

-- vlozenie, prvky v strome sa neopakuju
insertBVS      :: (Ord t) => t -> BVS t -> BVS t 
insertBVS x Nil = Node Nil x Nil      
insertBVS x (Node left value right) = if x < value then (Node (insertBVS x left) value right)
									  else if x == value then (Node left value right)
									  else (Node left value (insertBVS x right))

-- zmazanie
deleteBVS    :: (Ord t) => t -> BVS t -> BVS t
deleteBVS x Nil = Nil      
deleteBVS x (Node left value right) = if x < value then (Node (deleteBVS x left) value right)
									  else if x == value && right /= Nil then (Node left (minBVS right) (deleteBVS (minBVS right) right))
									  else if x == value && right == Nil then left
									  else (Node left value (deleteBVS x right))
{--
instance Arbitrary a => Arbitrary (BVS a) where
  arbitrary = frequency 
              [
                (1, return Nil )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]  						  
									  
									  
qch1 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> (isBVS(tree) == isBVSLinear(tree)))::Int->BVS Int->Property)
qch2 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree && (elem x (flat tree) == False)) ==> (flat tree == flat (deleteBVS x (insertBVS x tree))))::Int->BVS Int->Property)
qch3 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> elem x (flat(insertBVS x tree)))::Int->BVS Int->Property)
qch4 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> elem x (flat(deleteBVS x tree)) == False)::Int->BVS Int->Property)
qch5 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> isBVS(insertBVS x tree))::Int->BVS Int->Property)
qch6 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> isBVS(deleteBVS x tree))::Int->BVS Int->Property)
qch8 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree) ==> isBVS(foldl (\t -> \y -> insertBVS y t) (Nil) [1..x]))::Int->BVS Int->Property)
qch9 = quickCheckWith stdArgs{ maxSuccess = 1000 } ((\x -> \tree -> (isBVS tree && x > 1) ==> length (flat(foldl (\t -> \y -> insertBVS y t) (Nil) [1..x])) == x )::Int->BVS Int->Property)

qch7 tomuto som nerozumel, "x sa nachádza v strome, práve vtedy, ak sa nachádza vo flat stromu" --}

