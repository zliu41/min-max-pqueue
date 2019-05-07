{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- | A min-max priority queue implemented using a min-max heap
-- backed by a 'Seq', for benchmarking purposes.
module SeqQueue (fromList, pollMin) where

import qualified Data.Foldable as Foldable
import           Data.Maybe (catMaybes, fromJust)
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq, (|>), ViewL((:<)), ViewR((:>)))
import           Math.NumberTheory.Logarithms (intLog2)

import           Prelude hiding (init)

type SeqQueue a = Seq a
type Index = Int

empty :: SeqQueue a
empty = Seq.empty

fromList :: Ord a => [a] -> SeqQueue a
fromList = Foldable.foldr insert empty

size :: SeqQueue a -> Int
size = Seq.length

insert :: Ord a => a -> SeqQueue a -> SeqQueue a
insert a q = bubbleUp (size q') a q'
  where q' = q |> a

peekMin :: SeqQueue a -> Maybe a
peekMin q
  | hd :< _ <- Seq.viewl q = Just hd
  | otherwise = Nothing

deleteMin :: Ord a => SeqQueue a -> SeqQueue a
deleteMin q
  | init :> an <- Seq.viewr q =
      trickleDown 1 an (update 1 an init)
  | otherwise = empty

pollMin :: Ord a => SeqQueue a -> Maybe (a, SeqQueue a)
pollMin q = (,) <$> peekMin q <*> pure (deleteMin q)

(!) :: Seq a -> Index -> a
(!) xs i = fromJust $ Seq.lookup (i-1) xs

(!?) :: Seq a -> Index -> Maybe a
(!?) xs i = Seq.lookup (i-1) xs

bubbleUp :: Ord a => Index -> a -> SeqQueue a -> SeqQueue a
bubbleUp idx a q
    | idx == 1 = q
    | isMinLevel idx =
        if a > parent
          then bubbleUpMax parentIdx a (swap parentIdx parent idx a q)
          else bubbleUpMin idx a q
    | otherwise =
        if a < parent
          then bubbleUpMin parentIdx a (swap parentIdx parent idx a q)
          else bubbleUpMax idx a q
  where
    parentIdx = idx `div` 2
    parent = q ! parentIdx

bubbleUpMin :: Ord a => Index -> a -> SeqQueue a -> SeqQueue a
bubbleUpMin idx a q
    | idx < 4 = q
    | a < grandParent = bubbleUpMin grandParentIdx a (swap grandParentIdx grandParent idx a q)
    | otherwise = q
  where
    grandParentIdx = idx `div` 4
    grandParent = q ! grandParentIdx

bubbleUpMax :: Ord a => Index -> a -> SeqQueue a -> SeqQueue a
bubbleUpMax idx a q
    | idx < 4 = q
    | a > grandParent = bubbleUpMax grandParentIdx a (swap grandParentIdx grandParent idx a q)
    | otherwise = q
  where
    grandParentIdx = idx `div` 4
    grandParent = q ! grandParentIdx

isMinLevel :: Int -> Bool
isMinLevel = even . intLog2

swap :: Index -> a -> Index -> a -> SeqQueue a -> SeqQueue a
swap idx1 a1 idx2 a2 = update idx1 a2 . update idx2 a1

trickleDown :: Ord a => Index -> a -> SeqQueue a -> SeqQueue a
trickleDown idx a q
  | fmly@(_:_) <- family idx q =
      let (a',idx') = Foldable.minimum fmly
       in if a' >= a
            then q
            else if idx' > 2 * idx + 1
                   then let q' = swap idx' a' idx a q
                            parentIdx = idx' `div` 2
                            parent = fst . fromJust $ Foldable.find ((== parentIdx) . snd) fmly
                         in if a > parent then trickleDown idx' parent (swap parentIdx parent idx' a q') else trickleDown idx' a q'
                   else swap idx' a' idx a q
  | otherwise = q

family :: Index -> SeqQueue a -> [(a, Index)]
family idx q = catMaybes
    [ (,l) <$> (q !? l)
    , (,r) <$> (q !? r)
    , (,ll) <$> (q !? ll)
    , (,lr) <$> (q !? lr)
    , (,rl) <$> (q !? rl)
    , (,rr) <$> (q !? rr)
    ]
  where
    l = idx * 2
    r = l + 1
    ll = idx * 4
    lr = ll + 1
    rl = ll + 2
    rr = ll + 3

update :: Int -> a -> SeqQueue a -> SeqQueue a
update x = Seq.update (x-1)
