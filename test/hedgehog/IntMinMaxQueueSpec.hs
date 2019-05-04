{-# LANGUAGE TemplateHaskell #-}

module IntMinMaxQueueSpec where

import qualified Data.List as List

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Data.IntMinMaxQueue (IntMinMaxQueue)
import qualified Data.IntMinMaxQueue as PQ

genQueue :: Gen (IntMinMaxQueue Int)
genQueue = do
  xs <- Gen.list (Range.linear 0 500) (Gen.int (Range.linear 0 50))
  pure $ PQ.fromListWith id xs

prop_ascendingOrder :: Property
prop_ascendingOrder = property $ do
  q <- forAll genQueue
  let ascList = dequeueAllAsc q
  ascList === List.sort ascList
  PQ.size q === length ascList

prop_descendingOrder :: Property
prop_descendingOrder = property $ do
  q <- forAll genQueue
  let descList = dequeueAllDesc q
  descList === List.sortBy (flip compare) descList
  PQ.size q === length descList

prop_takeMin :: Property
prop_takeMin = property $ do
  q <- forAll genQueue
  n <- forAll $ Gen.int (Range.linear (-100) (PQ.size q + 100))
  take n (dequeueAllAsc q) === dequeueAllAsc (PQ.takeMin n q)
  length (take n (dequeueAllAsc q)) === PQ.size (PQ.takeMin n q)

prop_takeMax :: Property
prop_takeMax = property $ do
  q <- forAll genQueue
  n <- forAll $ Gen.int (Range.linear (-100) (PQ.size q + 100))
  take n (dequeueAllDesc q) === dequeueAllDesc (PQ.takeMax n q)
  length (take n (dequeueAllDesc q)) === PQ.size (PQ.takeMax n q)

prop_dropMin :: Property
prop_dropMin = property $ do
  q <- forAll genQueue
  n <- forAll $ Gen.int (Range.linear (-100) (PQ.size q + 100))
  drop n (dequeueAllAsc q) === dequeueAllAsc (PQ.dropMin n q)
  length (drop n (dequeueAllAsc q)) === PQ.size (PQ.dropMin n q)

prop_dropMax :: Property
prop_dropMax = property $ do
  q <- forAll genQueue
  n <- forAll $ Gen.int (Range.linear (-100) (PQ.size q + 100))
  drop n (dequeueAllDesc q) === dequeueAllDesc (PQ.dropMax n q)
  length (drop n (dequeueAllDesc q)) === PQ.size (PQ.dropMax n q)

prop_maxSize :: Property
prop_maxSize = property $ do
  q <- forAll genQueue
  n <- forAll $ Gen.int (Range.linear (-100) (PQ.size q + 100))
  let q' = q `PQ.withMaxSize` n
  PQ.size q' === max 0 (min (PQ.size q) n)
  PQ.maxSize q' === Just (max 0 n)
  dequeueAllAsc q' === take (PQ.size q') (dequeueAllAsc q)

prop_insertWithMaxSize :: Property
prop_insertWithMaxSize = property $ do
  q <- forAll genQueue
  let q' = q `PQ.withMaxSize` PQ.size q
  PQ.insert id maxBound q' === q'
  PQ.insert id minBound q' === PQ.insert id minBound (PQ.deleteMax q')

dequeueAllAsc :: IntMinMaxQueue a -> [a]
dequeueAllAsc = List.unfoldr PQ.pollMin

dequeueAllDesc :: IntMinMaxQueue a -> [a]
dequeueAllDesc = List.unfoldr PQ.pollMax

tests :: IO Bool
tests =
  checkParallel $$(discover)
