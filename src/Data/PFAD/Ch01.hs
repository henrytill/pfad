-- |
-- Module      : Data.PFAD.Ch01
-- Description : The smallest free number
--
module Data.PFAD.Ch01 where

-- * An array-based solution

minfree :: (Num a, Eq a, Enum a) => [a] -> a
minfree xs = head ([0..] \\ xs)

(\\) :: Eq a => [a] -> [a] -> [a]
us \\ vs = filter (flip notElem vs) us
