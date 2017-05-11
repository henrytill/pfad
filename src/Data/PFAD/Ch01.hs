-- |
-- Module      : Data.PFAD.Ch01
-- Description : The smallest free number
--
module Data.PFAD.Ch01 where

-- * An array-based solution

-- | Computes the smallest number not in a given finite set.
--
-- >>> minfree [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
-- 15
--
minfree :: (Num a, Eq a, Enum a) => [a] -> a
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (flip notElem vs) us
