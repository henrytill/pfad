module Main (main) where

import Test.DocTest

testFiles :: [String]
testFiles =
  [ "src/Data/PFAD/Ch01.hs",
    "src/Data/PFAD/Ch02.hs"
  ]

main :: IO ()
main = doctest (["-isrc"] ++ testFiles)
