module Jan19 where

import Data.List (partition)

--Quicksort
--pick a  pivot
--partition rest of elements in < and >=
--recrusively sort
--put lists back together
qs :: Ord a => [a] -> [a]
qs [] = []
qs (x : xs) = (qs l) ++ (x : qs r)
    where (l, r) = partition (< x) xs

--Insertion Sort
--for every element in the source list,
--insert it 'in the right place' in the sorted output list
insert:: Ord a  => a -> [a] -> [a]
insert x [] = [x]
insert x l@(y : ys) | x < y = x : l
                  | otherwise = y : insert x ys

insertionsort  :: Ord a => [a] -> [a]
insertionsort l = foldr insert [] l