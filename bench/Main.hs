module Main (main) where

import Criterion.Main
import qualified Data.PFAD.Ch01 as Ch01
import qualified Data.PFAD.Ch02 as Ch02

-- * Chapter 1

mftd20 = [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]

mftd4 = take 4 mftd20

mftd8 = take 8 mftd20

mftd12 = take 12 mftd20

mftd16 = take 16 mftd20

minfree1 =
  bgroup
    "minfree1"
    [ bench "mftd4" $ whnf Ch01.minfree1 mftd4,
      bench "mftd8" $ whnf Ch01.minfree1 mftd8,
      bench "mftd12" $ whnf Ch01.minfree1 mftd12,
      bench "mftd16" $ whnf Ch01.minfree1 mftd16,
      bench "mftd20" $ whnf Ch01.minfree1 mftd20
    ]

minfree2 =
  bgroup
    "minfree2"
    [ bench "mftd4" $ whnf Ch01.minfree2 mftd4,
      bench "mftd8" $ whnf Ch01.minfree2 mftd8,
      bench "mftd12" $ whnf Ch01.minfree2 mftd12,
      bench "mftd16" $ whnf Ch01.minfree2 mftd16,
      bench "mftd20" $ whnf Ch01.minfree2 mftd20
    ]

minfree4 =
  bgroup
    "minfree4"
    [ bench "mftd4" $ whnf Ch01.minfree4 mftd4,
      bench "mftd8" $ whnf Ch01.minfree4 mftd8,
      bench "mftd12" $ whnf Ch01.minfree4 mftd12,
      bench "mftd16" $ whnf Ch01.minfree4 mftd16,
      bench "mftd20" $ whnf Ch01.minfree4 mftd20
    ]

minfree6 =
  bgroup
    "minfree6"
    [ bench "mftd4" $ whnf Ch01.minfree6 mftd4,
      bench "mftd8" $ whnf Ch01.minfree6 mftd8,
      bench "mftd12" $ whnf Ch01.minfree6 mftd12,
      bench "mftd16" $ whnf Ch01.minfree6 mftd16,
      bench "mftd20" $ whnf Ch01.minfree6 mftd20
    ]

minfree7 =
  bgroup
    "minfree7"
    [ bench "mftd4" $ whnf Ch01.minfree7 mftd4,
      bench "mftd8" $ whnf Ch01.minfree7 mftd8,
      bench "mftd12" $ whnf Ch01.minfree7 mftd12,
      bench "mftd16" $ whnf Ch01.minfree7 mftd16,
      bench "mftd20" $ whnf Ch01.minfree7 mftd20
    ]

minfrees =
  [ minfree1,
    minfree2,
    minfree4,
    minfree6,
    minfree7
  ]

-- * Chapter 2

mscd = "GENERATING"

msc1 = bgroup "msc1" [bench "msc1" $ whnf Ch02.msc1 mscd]

msc2 = bgroup "msc2" [bench "msc2" $ whnf Ch02.msc2 mscd]

mscs =
  [ msc1,
    msc2
  ]

main :: IO ()
main = defaultMain (minfrees ++ mscs)
