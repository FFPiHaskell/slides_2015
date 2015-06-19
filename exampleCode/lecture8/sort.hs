module Main where

import Data.List
import Test.QuickCheck

-- Sorting twice changes nothing
prop_idempotency :: Ord a => [a] -> Bool
prop_idempotency xs = qsort xs == qsort (qsort xs)

-- Sorting doesn't change the length
prop_len :: Ord a => [a] -> Bool
prop_len xs = length xs == length (qsort xs)

-- Sorted result is a permutation of input
prop_perm :: Ord a => [a] -> Bool
prop_perm xs = (qsort xs) `elem` (permutations xs)

-- Sorting produces sorted list
prop_sort :: Ord a => [a] -> Bool
prop_sort = isSorted . qsort
  where
    isSorted :: Ord a => [a] -> Bool
    isSorted []       = True
    isSorted [x]      = True
    isSorted (x:y:zs) = (x <= y) && isSorted (y:zs)

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (p:xs) = (qsort lesser) ++ p:(qsort greater)
  where
    lesser  = filter (< p) xs
    greater = filter (>= p) xs
