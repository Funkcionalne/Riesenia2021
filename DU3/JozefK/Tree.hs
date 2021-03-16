module Tree where

--import Test.QuickCheck
--import Text.Show.Functions
--import Data.List(sort)
import Data.List

{-
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Property (forAllShrink)
--import Data.Map hiding (map,null,filter,foldr)
--import Data.List (nub,delete)
--import Data.Data
--import Data.Char
import Control.Monad
--import Control.Monad.State
--import Data.Maybe (maybeToList)

import Test.HUnit
import System.Random
import System.IO.Unsafe

instance Arbitrary a => Arbitrary (BVS a) where
  arbitrary = frequency 
              [
                (1, return Nil )
              , (1, liftM3 Node arbitrary arbitrary arbitrary)
              ]
-}
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
maxBVS (Node _ value Nil) = value
maxBVS (Node _ _ right) = maxBVS right

-- minimalny prvok neprazdneho binarneho vyhladavacieho stromu, ak splna podmienku ...
minBVS    :: (Ord t) => BVS t -> t
minBVS (Node Nil value _) = value
minBVS (Node left _ _) = maxBVS left

-- linearna verzia isBVS, strom prejde len raz (+raz vytvorené pole :))

isBVSLinear    :: (Ord t) => BVS t -> Bool
isBVSLinear tree = checkOrder (flat tree)

checkOrder :: (Ord t) => [t] -> Bool
checkOrder [] = True
checkOrder [x] = True
checkOrder (x:y:xs) | x >= y = False --musi byť rovnosť aj keď pri inserte je napísané že sa prvky nemajú opakovať...
                    | otherwise = checkOrder (y:xs)


-- vlozenie, prvky v strome sa neopakuju
insertBVS      :: (Ord t) => t -> BVS t -> BVS t
insertBVS x Nil = (Node Nil x Nil)   
insertBVS x (Node left value right) | value == x = (Node left value right) 
                                    | value > x = (Node (insertBVS x left) value right)
                                    | otherwise = (Node left value (insertBVS x right))                                    

-- zmazanie
deleteBVS    :: (Ord t) => t -> BVS t -> BVS t
deleteBVS x Nil = Nil
deleteBVS x (Node left value right) | value == x = if left == Nil then right else (Node newLeft newValue right) 
                                    | value < x = (Node left value (deleteBVS x right))
                                    | otherwise = (Node (deleteBVS x left) value right)
                                      where (newLeft, newValue) = deleteNodeMaxo left
                                      

deleteNodeMaxo :: (Ord t) => BVS t -> (BVS t, t)
deleteNodeMaxo (Node left value Nil) = (left, value)
deleteNodeMaxo (Node left value right) = ((Node left value new), value) 
                                     where (new, _)= deleteNodeMaxo right    


--QUICKCHECKS (snáď v pohode, moc sa mi to do toho nechcelo :) hlavne keby si videla ako komplikovane to musim spušťať (keď už to konečne ide)):  
{-
qch1 = quickCheck((\tree -> (isBVS tree) == (isBVSLinear tree))::BVS Int -> Bool)
qch2 = quickCheck((\x -> \tree -> ((not (findBVS x tree)) &&  (isBVSLinear tree))  ==> (flat tree) == (flat $ deleteBVS x (insertBVS x tree)))::Int->BVS Int->Property)
qch3 = quickCheck((\x -> \tree -> ((not (findBVS x tree)) &&  (isBVSLinear tree))  ==> (findBVS x (insertBVS x tree)))::Int->BVS Int->Property)
qch4 = quickCheck((\x -> \tree -> ((findBVS x tree) &&  (isBVSLinear tree))  ==> (not(findBVS x (deleteBVS x tree))))::Int->BVS Int->Property)
qch5 = quickCheck((\x -> \tree -> ((not (findBVS x tree)) &&  (isBVSLinear tree))  ==> (isBVSLinear (insertBVS x tree)))::Int->BVS Int->Property)
qch6 = quickCheck((\x -> \tree -> ((findBVS x tree) &&  (isBVSLinear tree))  ==> (isBVSLinear (deleteBVS x tree)))::Int->BVS Int->Property)
qch7 = quickCheck((\x -> \tree -> ((findBVS x tree) &&  (isBVSLinear tree))  ==> (x `elem` (flat tree)))::Int->BVS Int->Property) -- + keď tak aj opačnú implikáciu...
qch8 = quickCheck((\x -> \n -> (n > 0)  ==> (isBVSLinear(foldr (\i -> \t -> insertBVS i t) (Nil) [1..n])))::Int->Int->Property)
qch9 = quickCheck((\x -> \n -> (n > 0)  ==> (length (flat (foldr (\i -> \t -> insertBVS i t) (Nil) [1..n])) == n))::Int->Int->Property)
-}