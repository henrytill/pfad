module Main (main) where

import Test.DocTest

testFiles :: [String]
testFiles =
  [ "src/Data/PFAD/Ch01.hs" ]

main :: IO ()
main = doctest (["-isrc"] ++ testFiles)
