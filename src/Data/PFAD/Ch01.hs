-- |
-- Module      : Data.PFAD.Ch01
-- Description : The smallest free number
module Data.PFAD.Ch01 where

import Data.Array
import Data.Array.ST
import Data.List (partition)

-- * An array-based solution

-- | Computes the smallest number not in a given finite set.
--
-- >>> minfree1 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree1 :: (Num a, Eq a, Enum a) => [a] -> a
minfree1 xs = head ([0 ..] \\ xs)

-- | Finds the elements of @us@ that are not present in @vs@
--
-- >>> [1, 2, 3, 4] \\ [2, 4, 6, 8]
-- [1,3]
(\\) :: (Eq a) => [a] -> [a] -> [a]
us \\ vs = filter (flip notElem vs) us

-- ** The key fact...

-- $
-- ... is that not every number in the range @[0..length xs]@ can be in @xs@.
--
-- The smallest number not in @xs@ is also the smallest number not in @filter
-- (<= n) xs@ where @n = length xs@.
--
-- >>> let xs = [0, 1, 2, 3, 5, 8]
-- >>> let n = length xs
-- >>> filter (<= n) xs
-- [0,1,2,3,5]
--
-- So, we build 'checklist' of those numbers present in @filter (<= n) xs@.

-- | Computes the smallest number not in a given finite set (more efficient
-- version).
--
-- >>> minfree2 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree2 :: [Int] -> Int
minfree2 = search . checklist

-- | Searches for the first element in a 'checklist' which is 'False'
--
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
search :: Array Int Bool -> Int
search = length . takeWhile id . elems

-- | Builds a 'checklist' of those numbers present in @filter (<= n) xs@ where
-- @n = length xs@.
--
-- A 'checklist' is a Boolean array with @n + 1@ slots, numbered from @0@ to
-- @n@, whose initial entries are everywhere 'False'.
--
-- For each element @x@ in @xs@ and provided @x <= n@, we set the array element
-- at position @x@ to 'True'.
--
-- >>> checklist [0]
-- array (0,1) [(0,True),(1,False)]
-- >>> checklist [0, 1]
-- array (0,2) [(0,True),(1,True),(2,False)]
-- >>> checklist [0, 1, 2]
-- array (0,3) [(0,True),(1,True),(2,True),(3,False)]
-- >>> checklist [0, 1, 3]
-- array (0,3) [(0,True),(1,True),(2,False),(3,True)]
checklist :: [Int] -> Array Int Bool
checklist xs = accumArray (||) False (0, n) (zip (filter (<= n) xs) (repeat True))
  where
    n = length xs

countlist :: [Int] -> Array Int Int
countlist xs = accumArray (+) 0 (0, n) (zip xs (repeat 1))
  where
    n = maximum xs

sort :: [Int] -> [Int]
sort xs = concat [replicate k x | (x, k) <- assocs (countlist xs)]

searchCount :: Array Int Int -> Int
searchCount = length . takeWhile (== 1) . elems

-- | Computes the smallest number not in a given finite set (alternate version).
--
-- >>> minfree3 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree3 :: [Int] -> Int
minfree3 = searchCount . countlist

checklistST :: [Int] -> Array Int Bool
checklistST xs = runSTArray $ do
  a <- newArray (0, n) False
  _ <- sequence [writeArray a x True | x <- xs, x <= n]
  return a
  where
    n = length xs

-- | Computes the smallest number not in a given finite set (procedural version).
--
-- >>> minfree4 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree4 :: [Int] -> Int
minfree4 = search . checklistST

-- * A divide and conquer solution

minfrom1 :: Int -> [Int] -> Int
minfrom1 a xs = head ([a ..] \\ xs)

minfree5 :: [Int] -> Int
minfree5 = minfrom1 0

minfrom2 :: Int -> [Int] -> Int
minfrom2 a xs
  | null xs = a
  | length us == b - a = minfrom2 b vs
  | otherwise = minfrom2 a us
  where
    (us, vs) = partition (< b) xs
    b = a + 1 + n `div` 2
    n = length xs

-- | Computes the smallest number not in a given finite set (a divide and
-- conquer version).
--
-- >>> minfree6 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree6 :: [Int] -> Int
minfree6 = minfrom2 0

-- | Computes the smallest number not in a given finite set (an optimized,
-- divide and conquer version).
--
-- >>> minfree7 [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
minfree7 :: [Int] -> Int
minfree7 xs = minfrom3 0 (length xs, xs)

minfrom3 :: Int -> (Int, [Int]) -> Int
minfrom3 a (n, xs)
  | n == 0 = a
  | m == b - a = minfrom3 b (n - m, vs)
  | otherwise = minfrom3 a (m, us)
  where
    (us, vs) = partition (< b) xs
    b = a + 1 + n `div` 2
    m = length us
