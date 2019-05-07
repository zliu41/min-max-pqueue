{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.IntMinMaxQueue
-- Maintainer  :  Ziyang Liu <free@cofree.io>
--
-- Double-ended priority queues where priority values are integers, allowing
-- efficient retrieval and removel from both ends of the queue.
--
-- A queue can be configured with a maximum size. Each time an insertion
-- causes the queue to grow beyond the size limit, the greatest element
-- will be automatically removed (rather than rejecting the insertion).
--
-- The implementation is backed by an @'IntMap' ('NonEmpty' a)@. This means
-- that certain operations, including 'peekMin', 'peekMax' and 'fromList',
-- are asymptotically more expensive than a mutable array based implementation.
-- In a pure language like Haskell, a
-- mutable array based implementation would be impure
-- and need to operate inside monads. And in many applications, regardless
-- of language, the additional time complexity would be a small or negligible
-- price to pay to avoid destructive updates anyway.
--
-- If you only access one end of the queue (i.e., you need a regular
-- priority queue), an implementation based on a kind of heap that is more
-- amenable to purely functional implementations, such as binomial heap
-- and pairing heap, is /potentially/ more efficient. But always benchmark
-- if performance is important; in my experience @Map@ /always/ wins, even for
-- regular priority queues.
--
-- See <https://github.com/zliu41/min-max-pqueue/blob/master/README.md README.md>
-- for more information.
module Data.IntMinMaxQueue (
  -- * IntMinMaxQueue type
    IntMinMaxQueue
  , Prio

  -- * Construction
  , empty
  , singleton
  , fromList
  , fromListWith
  , fromMap

  -- * Size
  , null
  , notNull
  , size

  -- * Maximum size
  , withMaxSize
  , maxSize

  -- * Queue operations
  , insert
  , peekMin
  , peekMax
  , deleteMin
  , deleteMax
  , pollMin
  , pollMax
  , takeMin
  , takeMax
  , dropMin
  , dropMax

  -- * Traversal
  -- ** Map
  , map
  , mapWithPriority

  -- ** Folds
  , foldr
  , foldl
  , foldrWithPriority
  , foldlWithPriority
  , foldMapWithPriority

  -- ** Strict Folds
  , foldr'
  , foldl'
  , foldrWithPriority'
  , foldlWithPriority'

  -- * Lists
  , elems
  , toList
  , toAscList
  , toDescList

  -- * Maps
  , toMap
  ) where

import           Data.Data (Data)
import qualified Data.Foldable as Foldable
import           Data.Functor.Classes
import           Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as Map
import           Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as Nel

import Prelude hiding (drop, foldl, foldr, lookup, map, null, take)
import qualified Prelude

type Size = Int
type MaxSize = Maybe Int
type Prio = Int

-- | A double-ended priority queue whose elements are compared
-- on an 'Int' field.
data IntMinMaxQueue a = IntMinMaxQueue {-# UNPACK #-} !Size !MaxSize !(IntMap (NonEmpty a))
  deriving (Eq, Ord, Data)

instance Eq1 IntMinMaxQueue where
  liftEq eqv q1 q2 =
    Map.size (toMap q1) == Map.size (toMap q2)
      && liftEq (liftEq eqv) (toList q1) (toList q2)

instance Ord1 IntMinMaxQueue where
  liftCompare cmpv q1 q2 =
    liftCompare (liftCompare cmpv) (toList q1) (toList q2)

instance Show a => Show (IntMinMaxQueue a) where
  showsPrec d q = showParen (d > 10) $
    showString "fromList " . shows (toList q)

instance Show1 IntMinMaxQueue where
  liftShowsPrec spv slv d m =
      showsUnaryWith (liftShowsPrec sp sl) "fromList" d (toList m)
    where
      sp = liftShowsPrec spv slv
      sl = liftShowList spv slv

instance Read a => Read (IntMinMaxQueue a) where
  readsPrec p = readParen (p > 10) $ \r -> do
    ("fromList",s) <- lex r
    (xs,t) <- reads s
    pure (fromList xs,t)

instance Read1 IntMinMaxQueue where
  liftReadsPrec rp rl = readsData $
      readsUnaryWith (liftReadsPrec rp' rl') "fromList" fromList
    where
      rp' = liftReadsPrec rp rl
      rl' = liftReadList rp rl

instance Functor IntMinMaxQueue where
  fmap = map

instance Foldable.Foldable IntMinMaxQueue where
  foldMap = foldMapWithPriority . const

-- | /O(1)/. The empty queue.
empty :: IntMinMaxQueue a
empty = IntMinMaxQueue 0 Nothing Map.empty

-- | /O(1)/. A queue with a single element.
singleton :: (a -> Prio) -> a -> IntMinMaxQueue a
singleton f a = IntMinMaxQueue 1 Nothing (Map.singleton (f a) (pure a))

-- | /O(n * log n)/. Build a queue from a list of (priority, element) pairs.
fromList :: [(Prio, a)] -> IntMinMaxQueue a
fromList = Foldable.foldr (uncurry (insert . const)) empty

-- | /O(n * log n)/. Build a queue from a list of elements and a function
-- from elements to priorities.
fromListWith :: (a -> Prio) -> [a] -> IntMinMaxQueue a
fromListWith f = Foldable.foldr (insert f) empty

-- | /O(n)/ (due to calculating the queue size).
fromMap :: IntMap (NonEmpty a) -> IntMinMaxQueue a
fromMap m = IntMinMaxQueue (sum (fmap length m)) Nothing m

-- | /O(1)/. Is the queue empty?
null :: IntMinMaxQueue a -> Bool
null = (== 0) . size

-- | /O(1)/. Is the queue non-empty?
notNull :: IntMinMaxQueue a -> Bool
notNull = not . null

-- | /O(1)/. The total number of elements in the queue.
size :: IntMinMaxQueue a -> Int
size (IntMinMaxQueue sz _ _) = sz

-- | Return a queue that is limited to the given number of elements.
-- If the original queue has more elements than the size limit, the greatest
-- elements will be dropped until the size limit is satisfied.
withMaxSize :: IntMinMaxQueue a -> Int -> IntMinMaxQueue a
withMaxSize q ms = IntMinMaxQueue sz (Just ms) m
  where (IntMinMaxQueue sz _ m) = takeMin ms q

-- | /O(1)/. The size limit of the queue. It returns either @Nothing@ (if
-- the queue does not have a size limit) or @Just n@ where @n >= 0@.
maxSize :: IntMinMaxQueue a -> Maybe Int
maxSize (IntMinMaxQueue _ ms _) = max 0 <$> ms

-- | /O(log n)/. Add the given element to the queue. If the queue has
-- a size limit, and the insertion causes the queue to grow beyond
-- its size limit, the greatest element will be removed from the
-- queue, which may be the element just added.
insert :: (a -> Prio) -> a -> IntMinMaxQueue a -> IntMinMaxQueue a
insert f a q@(IntMinMaxQueue sz ms _) = case ms of
  Just ms' | sz >= ms' -> deleteMax (insert' f a q)
  _ -> insert' f a q

insert' :: (a -> Prio) -> a -> IntMinMaxQueue a -> IntMinMaxQueue a
insert' f a (IntMinMaxQueue sz ms m) = IntMinMaxQueue (sz+1) ms (Map.alter g (f a) m)
  where
    g Nothing = Just (pure a)
    g (Just as) = Just (a <| as)

-- | /O(log n)/. Retrieve the least element of the queue, if exists.
peekMin :: IntMinMaxQueue a -> Maybe a
peekMin (IntMinMaxQueue _ _ m) = Nel.head . snd <$> Map.lookupMin m

-- | /O(log n)/. Retrieve the greatest element of the queue, if exists.
peekMax :: IntMinMaxQueue a -> Maybe a
peekMax (IntMinMaxQueue _ _ m) = Nel.head . snd <$> Map.lookupMax m

-- | /O(log n)/. Remove the least element of the queue, if exists.
deleteMin :: IntMinMaxQueue a -> IntMinMaxQueue a
deleteMin q@(IntMinMaxQueue sz ms m)
  | Just (prio,_) <- Map.lookupMin m = IntMinMaxQueue (sz-1) ms (Map.update (Nel.nonEmpty . Nel.tail) prio m)
  | otherwise = q

-- | /O(log n)/. Remove the greatest element of the queue, if exists.
deleteMax :: IntMinMaxQueue a -> IntMinMaxQueue a
deleteMax q@(IntMinMaxQueue sz ms m)
  | Just (prio,_) <- Map.lookupMax m = IntMinMaxQueue (sz-1) ms (Map.update (Nel.nonEmpty . Nel.tail) prio m)
  | otherwise = q

-- | /O(log n)/. Remove and return the least element of the queue, if exists.
pollMin :: IntMinMaxQueue a -> Maybe (a, IntMinMaxQueue a)
pollMin q = (,) <$> peekMin q <*> pure (deleteMin q)

-- | /O(log n)/. Remove and return the greatest element of the queue, if exists.
pollMax :: IntMinMaxQueue a -> Maybe (a, IntMinMaxQueue a)
pollMax q = (,) <$> peekMax q <*> pure (deleteMax q)

-- | @'takeMin' n q@ returns a queue with the @n@ least elements in @q@, or
-- @q@ itself if @n >= 'size' q@.
takeMin :: Int -> IntMinMaxQueue a -> IntMinMaxQueue a
takeMin n q@(IntMinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 <= sz = IntMinMaxQueue newSz ms (take Map.lookupMin newSz m)
    | otherwise = IntMinMaxQueue newSz ms (drop Map.lookupMax (sz - newSz) m)
  where newSz = max 0 (min sz n)

-- | @'takeMin' n q@ returns a queue with the @n@ greatest elements in @q@, or
-- @q@ itself if @n >= 'size' q@.
takeMax :: Int -> IntMinMaxQueue a -> IntMinMaxQueue a
takeMax n q@(IntMinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 <= sz = IntMinMaxQueue newSz ms (take Map.lookupMax newSz m)
    | otherwise = IntMinMaxQueue newSz ms (drop Map.lookupMin (sz - newSz) m)
  where newSz = max 0 (min sz n)

-- | @'dropMin' n q@ returns a queue with the @n@ least elements
-- dropped from @q@, or 'empty' if @n >= 'size' q@.
dropMin :: Int -> IntMinMaxQueue a -> IntMinMaxQueue a
dropMin n q@(IntMinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 > sz = IntMinMaxQueue newSz ms (drop Map.lookupMin (sz - newSz) m)
    | otherwise = IntMinMaxQueue newSz ms (take Map.lookupMax newSz m)
  where newSz = max 0 (min sz (sz - n))

-- | @'dropMax' n q@ returns a queue with the @n@ greatest elements
-- dropped from @q@, or 'empty' if @n >= 'size' q@.
dropMax :: Int -> IntMinMaxQueue a -> IntMinMaxQueue a
dropMax n q@(IntMinMaxQueue sz ms m)
    | newSz >= sz = q
    | newSz * 2 > sz = IntMinMaxQueue newSz ms (drop Map.lookupMax (sz - newSz) m)
    | otherwise = IntMinMaxQueue newSz ms (take Map.lookupMin newSz m)
  where newSz = max 0 (min sz (sz - n))

take
  :: (forall b. IntMap b -> Maybe (Int, b))
  -> Int -> IntMap (NonEmpty a) -> IntMap (NonEmpty a)
take lookup n m = go 0 m Map.empty
  where
    go sz mIn mOut
      | sz >= n = mOut
      | Just (prio, hd :| tl) <- lookup mIn =
          let as = hd :| Prelude.take (n - sz - 1) tl
              len = Nel.length as
              mOut' = Map.insert prio as mOut
              mIn' = Map.delete prio mIn
           in go (sz + len) mIn' mOut'
      | otherwise = mOut

drop
  :: (forall b. IntMap b -> Maybe (Int, b))
  -> Int -> IntMap (NonEmpty a) -> IntMap (NonEmpty a)
drop lookup n = go 0
  where
    go sz mOut
      | sz >= n = mOut
      | Just (prio, hd :| tl) <- lookup mOut =
          let len = length tl + 1
           in if sz + len <= n
                then go (sz + len) (Map.delete prio mOut)
                else Map.insert prio (hd :| Prelude.drop (n - sz) tl) mOut
      | otherwise = mOut

-- | Map a function over all elements in the queue.
map :: (a -> b) -> IntMinMaxQueue a -> IntMinMaxQueue b
map = mapWithPriority . const

-- | Map a function over all elements in the queue.
mapWithPriority :: (Prio -> a -> b) -> IntMinMaxQueue a -> IntMinMaxQueue b
mapWithPriority f (IntMinMaxQueue sz ms m) =
  IntMinMaxQueue sz ms (Map.mapWithKey (fmap . f) m)

-- | Fold the elements in the queue using the given right-associative
-- binary operator, such that @'foldr' f z == 'Prelude.foldr' f z . 'elems'@.
foldr :: (a -> b -> b) -> b -> IntMinMaxQueue a -> b
foldr = foldrWithPriority . const

-- | Fold the elements in the queue using the given left-associative
-- binary operator, such that @'foldl' f z == 'Prelude.foldl' f z . 'elems'@.
foldl :: (a -> b -> a) -> a -> IntMinMaxQueue b -> a
foldl = foldlWithPriority . (const .)

-- | Fold the elements in the queue using the given right-associative
-- binary operator, such that
-- @'foldrWithPriority' f z == 'Prelude.foldr' ('uncurry' f) z . 'toAscList'@.
foldrWithPriority :: (Prio -> a -> b -> b) -> b -> IntMinMaxQueue a -> b
foldrWithPriority f b (IntMinMaxQueue _ _ m) = Map.foldrWithKey f' b m
  where
    f' = flip . Foldable.foldr . f

-- | Fold the elements in the queue using the given left-associative
-- binary operator, such that
-- @'foldlWithPriority' f z == 'Prelude.foldr' ('uncurry' . f) z . 'toAscList'@.
foldlWithPriority :: (a -> Prio -> b -> a) -> a -> IntMinMaxQueue b -> a
foldlWithPriority f a (IntMinMaxQueue _ _ m) = Map.foldlWithKey f' a m
  where
    f' = flip (Foldable.foldl . flip f)

-- | A strict version of 'foldr'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldr' :: (a -> b -> b) -> b -> IntMinMaxQueue a -> b
foldr' = foldrWithPriority' . const

-- | A strict version of 'foldl'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldl' :: (a -> b -> a) -> a -> IntMinMaxQueue b -> a
foldl' = foldlWithPriority' . (const .)

-- | A strict version of 'foldrWithPriority'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldrWithPriority' :: (Prio -> a -> b -> b) -> b -> IntMinMaxQueue a -> b
foldrWithPriority' f b (IntMinMaxQueue _ _ m) = Map.foldrWithKey' f' b m
  where
    f' = flip . Foldable.foldr . f

-- | A strict version of 'foldlWithPriority'. Each application of the
-- operator is evaluated before using the result in the next application.
-- This function is strict in the starting value.
foldlWithPriority' :: (a -> Prio -> b -> a) -> a -> IntMinMaxQueue b -> a
foldlWithPriority' f a (IntMinMaxQueue _ _ m) = Map.foldlWithKey' f' a m
  where
    f' = flip (Foldable.foldl' . flip f)

-- | Fold the elements in the queue using the given monoid, such that
-- @'foldMapWithPriority' f == 'Foldable.foldMap' (uncurry f) . 'elems'@.
foldMapWithPriority :: Monoid m => (Prio -> a -> m) -> IntMinMaxQueue a -> m
foldMapWithPriority f (IntMinMaxQueue _ _ m) =
  Map.foldMapWithKey (Foldable.foldMap . f) m

-- | Elements in the queue in ascending order of priority.
-- Elements with the same priority are returned in no particular order.
elems :: IntMinMaxQueue a -> [a]
elems (IntMinMaxQueue _ _ m) = Foldable.foldMap Nel.toList m

-- | An alias for 'toAscList'.
toList :: IntMinMaxQueue a -> [(Prio, a)]
toList = toAscList

-- | Convert the queue to a list in ascending order of priority.
-- Elements with the same priority are returned in no particular order.
toAscList :: IntMinMaxQueue a -> [(Prio, a)]
toAscList (IntMinMaxQueue _ _ m) =
  Map.toAscList m >>= uncurry (\prio -> fmap (prio,) . Nel.toList)

-- | Convert the queue to a list in descending order of priority.
-- Elements with the same priority are returned in no particular order.
toDescList :: IntMinMaxQueue a -> [(Prio, a)]
toDescList (IntMinMaxQueue _ _ m) =
  Map.toDescList m >>= uncurry (\prio -> fmap (prio,) . Nel.toList)

-- | /O(n)/. Convert the queue to an 'IntMap'.
toMap :: IntMinMaxQueue a -> IntMap (NonEmpty a)
toMap (IntMinMaxQueue _ _ m) = m
