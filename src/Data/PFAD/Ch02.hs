-- |
-- Module      : Data.PFAD.Ch02
-- Description : A surpassing problem
--
module Data.PFAD.Ch02 where

-- * Specification

-- | Finds the maximum /surpasser count/ of an element.
--
-- The /surpasser count/ of an element is the number of its /surpasser/s.
--
-- A /surpasser/ of an element of an list is a greater element to the right.
--
-- >>> msc1 "GENERATING"
-- 6
--
msc1 :: Ord a => [a] -> Int
msc1 xs = maximum [scount z zs | z : zs <- tails xs]

-- | Counts the number of elements in @xs@ that are greater than @x@.
--
-- >>> scount 1 [0,1,2,3,4,5]
-- 4
-- >>> scount 4 [0,1,2,3,4,5]
-- 1
--
scount :: Ord a => a -> [a] -> Int
scount x xs = length (filter (x <) xs)

-- | Returns the /nonempty/ tails of a nonempty list in decreasing order of
-- length.
--
-- >>> tails [0,1,2,3,4,5]
-- [[0,1,2,3,4,5],[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5]]
--
tails :: [a] -> [[a]]
tails [] = []
tails (x : xs) = (x : xs) : tails xs

-- * Divide and conquer

-- | Returns each member of list paired with its surpasser count.
--
-- >>> table [1, 2, 3, 4]
-- [(1,3),(2,2),(3,1),(4,0)]
-- >>> table [7, 3, 5, 1]
-- [(7,0),(3,1),(5,0),(1,0)]
--
table :: Ord a => [a] -> [(a, Int)]
table xs = [(z, scount z zs) | z : zs <- tails xs]

-- | Finds the maximum surpasser count of an element.
--
-- >>> msc2 "GENERATING"
-- 6
--
msc2 :: Ord a => [a] -> Int
msc2 = maximum . map snd . table
