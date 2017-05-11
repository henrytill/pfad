module Main (main) where

import Criterion.Main
import qualified Data.PFAD.Ch01 as Ch01

minfree =
  bgroup "minfree"
  [ bench "4"  $ whnf Ch01.minfree [08, 23, 09, 00]
  , bench "8"  $ whnf Ch01.minfree [08, 23, 09, 00, 12, 11, 01, 10]
  , bench "12" $ whnf Ch01.minfree [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04]
  , bench "16" $ whnf Ch01.minfree [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17]
  , bench "20" $ whnf Ch01.minfree [08, 23, 09, 00, 12, 11, 01, 10, 13, 07, 41, 04, 14, 21, 05, 17, 03, 19, 02, 06]
  ]

main :: IO ()
main = defaultMain [ minfree ]
