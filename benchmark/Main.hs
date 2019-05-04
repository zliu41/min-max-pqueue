module Main where

import qualified Criterion.Main as Criterion
import           Data.List (sort, sortBy, unfoldr)
import           System.Random

import qualified Data.IntMinMaxQueue as IPQ

bench :: [Criterion.Benchmark]
bench =
  [ Criterion.bench "intpq-asc-min" $
      Criterion.nf (unfoldr IPQ.pollMin) (IPQ.fromListWith id ascElems)
  , Criterion.bench "intpq-asc-max" $
      Criterion.nf (unfoldr IPQ.pollMax) (IPQ.fromListWith id ascElems)
  , Criterion.bench "intpq-rand-min" $
      Criterion.nf (unfoldr IPQ.pollMin) (IPQ.fromListWith id randomElems)
  , Criterion.bench "intpq-rand-max" $
      Criterion.nf (unfoldr IPQ.pollMax) (IPQ.fromListWith id randomElems)
  ]

main :: IO ()
main = Criterion.defaultMain bench

numElems :: Int
numElems = 200000

gen :: StdGen
gen = mkStdGen 42

randomElems :: [Int]
randomElems = take numElems (randoms gen)

ascElems :: [Int]
ascElems = sort randomElems

descElems :: [Int]
descElems = sortBy (flip compare) randomElems
