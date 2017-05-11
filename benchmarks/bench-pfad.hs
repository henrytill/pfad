module Main (main) where

import Criterion.Main
import qualified Data.PFAD.Ch01 as Ch01

mftd20 = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
mftd4  = take 4  mftd20
mftd8  = take 8  mftd20
mftd12 = take 12 mftd20
mftd16 = take 16 mftd20

minfree1 =
  bgroup "minfree1"
  [ bench "mftd4"  $ whnf Ch01.minfree1 mftd4
  , bench "mftd8"  $ whnf Ch01.minfree1 mftd8
  , bench "mftd12" $ whnf Ch01.minfree1 mftd12
  , bench "mftd16" $ whnf Ch01.minfree1 mftd16
  , bench "mftd20" $ whnf Ch01.minfree1 mftd20
  ]

minfree2 =
  bgroup "minfree2"
  [ bench "mftd4"  $ whnf Ch01.minfree2 mftd4
  , bench "mftd8"  $ whnf Ch01.minfree2 mftd8
  , bench "mftd12" $ whnf Ch01.minfree2 mftd12
  , bench "mftd16" $ whnf Ch01.minfree2 mftd16
  , bench "mftd20" $ whnf Ch01.minfree2 mftd20
  ]

main :: IO ()
main = defaultMain [ minfree1, minfree2 ]
