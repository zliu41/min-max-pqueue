module Main where

import qualified Criterion.Main as Criterion
import           Data.List (sort, sortBy, unfoldr)
import           System.Random

import qualified Data.IntMinMaxQueue as IPQ
import qualified Data.MinMaxQueue as PQ

bench :: [Criterion.Benchmark]
bench =
  [ Criterion.bench "intpq-asc-min" $
      Criterion.nf (unfoldr IPQ.pollMin) (IPQ.fromListWith id ascElems)
  , Criterion.bench "intpq-desc-max" $
      Criterion.nf (unfoldr IPQ.pollMax) (IPQ.fromListWith id descElems)
  , Criterion.bench "intpq-rand-min" $
      Criterion.nf (unfoldr IPQ.pollMin) (IPQ.fromListWith id randomElems)
  , Criterion.bench "intpq-rand-max" $
      Criterion.nf (unfoldr IPQ.pollMax) (IPQ.fromListWith id randomElems)

  , Criterion.bench "pq-asc-min" $
      Criterion.nf (unfoldr PQ.pollMin) (PQ.fromListWith id ascElems)
  , Criterion.bench "pq-desc-max" $
      Criterion.nf (unfoldr PQ.pollMax) (PQ.fromListWith id descElems)
  , Criterion.bench "pq-rand-min" $
      Criterion.nf (unfoldr PQ.pollMin) (PQ.fromListWith id randomElems)
  , Criterion.bench "pq-rand-max" $
      Criterion.nf (unfoldr PQ.pollMax) (PQ.fromListWith id randomElems)
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
