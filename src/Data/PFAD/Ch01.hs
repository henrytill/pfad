-- |
-- Module      : Data.PFAD.Ch01
-- Description : The smallest free number
--
module Data.PFAD.Ch01 where

import Data.Array

-- * An array-based solution

-- | Computes the smallest number not in a given finite set.
--
-- >>> minfree1 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
--
minfree1 :: (Num a, Eq a, Enum a) => [a] -> a
minfree1 xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (flip notElem vs) us

-- | Computes the smallest number not in a given finite set.
--
-- >>> minfree2 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
--
minfree2 ::  [Int] -> Int
minfree2 = search . checklist

-- |
-- >>> search (listArray (0, 2) [False, False, False])
-- 0
-- >>> search (listArray (0, 2) [False, True, False])
-- 0
-- >>> search (listArray (0, 2) [False, False, True])
-- 0
-- >>> search (listArray (0, 2) [True, False, False])
-- 1
-- >>> search (listArray (0, 2) [True, True, False])
-- 2
-- >>> search (listArray (0, 2) [True, False, True])
-- 1
-- >>> search (listArray (0, 2) [True, True, True])
-- 3
--
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- |
-- >>> checklist [0]
-- array (0,1) [(0,True),(1,False)]
-- >>> checklist [0, 1]
-- array (0,2) [(0,True),(1,True),(2,False)]
-- >>> checklist [0, 1, 2]
-- array (0,3) [(0,True),(1,True),(2,True),(3,False)]
-- >>> checklist [0, 1, 3]
-- array (0,3) [(0,True),(1,True),(2,False),(3,True)]
--
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where n = length xs
