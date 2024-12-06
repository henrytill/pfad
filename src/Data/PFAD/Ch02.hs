-- |
-- Module      : Data.PFAD.Ch02
-- Description : A surpassing problem
module Data.PFAD.Ch02 where

-- $setup
-- >>> import Test.QuickCheck

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
-- This definition takes quadratic time.
msc1 :: (Ord a) => [a] -> Int
msc1 xs = maximum [scount z zs | z : zs <- tails xs]

-- | Counts the number of elements in @xs@ that are greater than @x@.
--
-- >>> scount 1 [0,1,2,3,4,5]
-- 4
-- >>> scount 4 [0,1,2,3,4,5]
-- 1
scount :: (Ord a) => a -> [a] -> Int
scount x xs = length (filter (x <) xs)

-- | Returns the /nonempty/ tails of a nonempty list in decreasing order of
-- length.
--
-- >>> tails [0,1,2,3,4,5]
-- [[0,1,2,3,4,5],[1,2,3,4,5],[2,3,4,5],[3,4,5],[4,5],[5]]
tails :: [a] -> [[a]]
tails [] = []
tails (x : xs) = (x : xs) : tails xs

-- * Divide and conquer

-- $
-- The minimal generalisation is to start out with the table of /all/ surpasser
-- counts.

-- | Returns each member of list paired with its surpasser count.
--
-- >>> table1 [1, 2, 3, 4]
-- [(1,3),(2,2),(3,1),(4,0)]
-- >>> table1 [7, 3, 5, 1]
-- [(7,0),(3,1),(5,0),(1,0)]
table1 :: (Ord a) => [a] -> [(a, Int)]
table1 xs = [(z, scount z zs) | z : zs <- tails xs]

-- | Finds the maximum surpasser count of an element.
--
-- >>> msc2 "GENERATING"
-- 6
msc2 :: (Ord a) => [a] -> Int
msc2 = maximum . map snd . table1

-- $
-- We need the following divide and conquer property of `tails`
--
-- prop> tails (xs ++ ys) == map (++ ys) (tails xs) ++ tails ys

join1 :: (Ord a) => [(a, Int)] -> [(a, Int)] -> [(a, Int)]
join1 txs tys = [(z, c + tcount z tys) | (z, c) <- txs] ++ tys

tcount :: (Ord a) => a -> [(a, b)] -> Int
tcount z tys = scount z (map fst tys)

-- | A version of `tcount` that assumes @tys@ is sorted in ascending order
tcount2 z tys = length (dropWhile ((z >) . fst) tys)

-- ** Putting these results together...

-- | Returns each member of list paired with its surpasser count.
--
-- >>> table2 [1, 2, 3, 4]
-- [(1,3),(2,2),(3,1),(4,0)]
-- >>> table2 [7, 3, 5, 1]
-- [(1,0),(3,1),(5,0),(7,0)]
--
-- This definition takes /O (n log n)/.
--
-- In fact it is not possible to do better than an /O (n log n)/ algorithm.
table2 :: (Ord t) => [t] -> [(t, Int)]
table2 [x] = [(x, 0)]
table2 xs = join2 (m - n) (table2 ys) (table2 zs)
  where
    m = length xs
    n = m `div` 2
    (ys, zs) = splitAt n xs

-- | Joins two tables.  Takes the latter table's length as an argument to avoid
-- repeated calculations.
--
-- >>> join2 4 [(1,1),(4,2),(8,4),(12,6)] [(1,0),(3,1),(5,2),(7,3)]
-- [(1,0),(1,4),(3,1),(4,4),(5,2),(7,3),(8,4),(12,6)]
--
-- This definition takes linear time.
join2 ::
  (Eq a, Num a, Ord t) =>
  a ->
  [(t, a)] ->
  [(t, a)] ->
  [(t, a)]
join2 0 txs [] = txs
join2 n [] tys = tys
join2 n txs@((x, c) : txs') tys@((y, d) : tys')
  | x < y = (x, c + n) : join2 n txs' tys
  | x >= y = (y, d) : join2 (n - 1) txs tys'

-- | Finds the maximum surpasser count of an element.
--
-- >>> msc3 "GENERATING"
-- 6
--
-- This definition takes /O (n log n)/.
msc3 :: (Ord a) => [a] -> Int
msc3 = maximum . map snd . table2
