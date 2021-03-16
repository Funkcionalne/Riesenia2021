module Najcastejsi where

import Data.List

groupSort :: (Ord t) => [t] -> [[t]]
groupSort = (sortOn length) . group . sort

najcastejsi :: (Ord t) => [t] -> t
najcastejsi = head . last . groupSort

najzriedkavejsi :: (Ord t) => [t] -> t
najzriedkavejsi = head . head . groupSort

median :: (Integral t) => [t] -> t
median xs = sort xs !! ((length xs - 1) `div` 2)
